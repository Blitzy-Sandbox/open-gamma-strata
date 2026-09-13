#!/usr/bin/env bash
#
# verify-gates.sh - the acceptance-gate runner for this repository.
#
# PROVENANCE
#   Every row this script runs, and every row's pass condition, comes from the
#   validation table of the technical specification (AAP section 0.10.1). The
#   twenty automated rows are executed in that table's order - the closure row
#   beside the Rule 4 row whose source-level claim it completes - followed by
#   the one row that is reported rather than measured (Gate 7's manual approval,
#   which is an out-of-band pull-request review and never blocks this script).
#   No row may be relaxed, skipped or short-circuited, and nothing absent is
#   read as agreement: a row whose input artifact is missing, whose marker is
#   not there, or whose command failed, FAILS. Nothing else in the
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
#   scripts/verify-gates.sh                      run every gate, write the
#                                                report, publish the evidence
#   scripts/verify-gates.sh --verify-publication re-check target/publish
#                                                against the digests that were
#                                                approved, and exit non-zero if
#                                                it is not the tree that was
#                                                scanned. Read-only: it runs no
#                                                gate, takes no lock, creates
#                                                nothing and deletes nothing,
#                                                which is what lets the step
#                                                that uploads the tree run it.
#   scripts/verify-gates.sh -h|--help            print this usage and exit
#
#   Those are the only accepted invocations: no other argument is understood,
#   and no second argument is, so every other arity is a usage error rather
#   than a silently ignored word. That holds for the two flags as well -
#   neither `--verify-publication` nor the help flag takes an argument of its
#   own, and a word after either is rejected rather than discarded, because a
#   word this script ignored would be an instruction it did not carry out.
#   `classify_invocation` is the single place that decides this, and both
#   `main` (which needs the mode before any side effect) and `parse_arguments`
#   (which holds the side-effecting half) read its answer. There is
#   deliberately no flag that selects a subset of gates: a partial run is not
#   an acceptance run.
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
#   0  every automated row passed AND the evidence was published safely
#   1  one or more automated rows failed, or the evidence could not be
#      published safely (which fails a run whose rows all passed: the
#      artifacts are the deliverable)
#   2  usage error, a required tool is missing (preflight failure), an output
#      path was rejected, the evidence an earlier run left behind carries a
#      credential signature, or another acceptance run already holds this
#      checkout's output lock
#
#   With --verify-publication: 0 when target/publish is exactly the tree the
#   run approved, 1 when it is not, 2 when this is not a repository checkout.
#
# ARTIFACTS (all under $ROOT/target)
#   target/gate-report.md          the deliverable evidence: one row per gate,
#                                  each stating the command it was measured
#                                  with beside its verdict, detail and
#                                  evidence path; the run's identity - commit,
#                                  branch, run id and working-tree state - a
#                                  machine-readable summary line and the
#                                  appendices each row contributed. An earlier
#                                  run's report is replaced by an IN PROGRESS
#                                  stub when a run starts, so this file never
#                                  states a verdict on a run other than the
#                                  one it names
#   target/parity-report/*.json    the six parity reports the specs write
#   target/test-reports/TEST-*.xml the per-suite JUnit XML written by the one
#                                  ScalaTest `-u` reporter `build.sbt`
#                                  configures - the only test report this
#                                  build produces at the repository root
#   target/audit/                  per-row evidence, sbt logs, class-load logs,
#                                  run-identity.txt, the scan summaries and the
#                                  snapshots the late rows read
#   target/publish/                THE ONLY TREE CI UPLOADS: a sanitized copy
#                                  of each of the four above, every copy proved
#                                  to belong to this run and scanned for
#                                  credential signatures before it was kept,
#                                  with PUBLICATION-STATUS.txt stating APPROVED
#                                  or NOT APPROVED and MANIFEST.txt listing
#                                  what was staged, what was redacted from it
#                                  and what was withheld
#   target/quarantine/<run-id>/    artifacts withheld from publication because
#                                  they matched a credential signature, moved
#                                  out of every path anything uploads
#
#   Sanitized means: the JUnit `<properties>` block and the suite `hostname`
#   attribute are removed, and the checkout, home, temporary and toolchain
#   paths and the machine name are replaced by placeholders. The raw artifacts
#   stay on disk for whoever is at the machine; they are not what leaves it.
#
#   The report is written on every exit path, including an interruption, and
#   the publication tree is built on every exit path too - so an aborted or
#   interrupted run uploads scanned copies exactly as a finished one does.
#   There are two exceptions, both stated rather than silent: a rejected
#   output path (if target/ cannot be created safely there is nowhere to write
#   anything, so the run stops with exit 2 and the reason on stderr), and a
#   report that carries a credential signature its reassembly cannot remove
#   (it is quarantined instead of published, and the run exits non-zero).
#
# REQUIRED TOOLCHAIN
#   JDK 21 (`java`, `javap`) and sbt 1.13.0 on PATH, plus git, python3 and the
#   POSIX text utilities. `jq`, `xmllint` and `shellcheck` are neither required
#   nor used: every JSON, XML and CSV document is parsed with python3 and its
#   standard library only. On a small host export SBT_OPTS first, e.g.
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
#   * The three evidence trees are EMPTIED before the first row runs, after
#     what they held has been scanned and anything credential-bearing has been
#     quarantined. Provenance then needs no inference: everything found in them
#     afterwards was produced by this run. A modification time is metadata any
#     process can set, so "newer than the run started" was a guess; it survives
#     only as a second, independent check at staging time, together with a
#     refusal of any timestamp in the future.
#   * The approval of the published bytes is carried across the handover to
#     whoever uploads them: MANIFEST.txt records the sha256 of every published
#     copy, and `--verify-publication` re-checks the marker, those digests and
#     the absence of any file the manifest does not name. The CI job runs it
#     and replaces an unapproved or altered tree with a notice before the
#     upload step, so `when: always` cannot upload what the scan rejected.
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
#     Those pathname checks are the first refusal, not the decision: nothing
#     under target/ is created, written, emptied or deleted BY PATHNAME, and
#     that includes the WRITE and not only the check before it - a truncation
#     proved safe and then written through a second lookup of the same name is
#     the same race with an extra step. A whole-file write goes into a new
#     inode created O_EXCL inside the parent's descriptor and takes the name by
#     rename; an append goes through a descriptor whose properties were read
#     with fstat; and every row's evidence file is opened ONCE, proved to be
#     that file, and written through `/proc/self/fd/N` for the rest of the row.
#     `fs_guard`
#     re-opens every component no-follow from its parent's descriptor and
#     refuses a symbolic link, a hard-linked file, a foreign owner, a FIFO or
#     a non-directory before any write happens, which is what a check on a
#     name cannot do (CWE-367). target/audit is this run's own, and private.
#   * One acceptance run per checkout. `init_run` takes an exclusive lock -
#     the directory target/.gate-lock - before it creates or empties
#     anything, and the EXIT path releases it after the report is written. A
#     lock left behind by a process that is no longer running is reclaimed
#     and noted in target/audit/output-lock.txt; one held by a live process
#     stops this run with exit 2, because two runs share every evidence file
#     and the report that survived would describe neither of them.
#   * A value this script did not choose is ENCODED where it crosses into
#     something that parses it: `scala_string_literal` for the one path that
#     reaches Scala source through sbt's `set`, `markdown_cell` for every
#     field of the report and of the boundary row's material file. The
#     checkout path itself is refused outright by `assert_root_is_safe` if it
#     carries a quote, a backslash, a backtick, a dollar sign or a control
#     character, which is cheaper and stronger than encoding it correctly in
#     each of the places it is used. A SPACE is not in that list: it injects
#     nothing into Scala source or Markdown, every path expansion here is
#     quoted, and refusing it would make a checkout that merely sits in a
#     directory with a space in its name unverifiable.
#   * target/gate-report.md is the deliverable evidence, so it is written to a
#     temporary file, verified, SCANNED for credential signatures and only
#     then renamed atomically. Only a rename that succeeded marks the report
#     written, which is what lets the EXIT trap replace a partial one.
#   * Composing the report, staging it and verifying everything that would be
#     uploaded are ONE transaction (`finalize_publication`), and nothing is
#     marked published until the scan governing the uploaded bytes has passed.
#     A finding is acted on - the copy withdrawn, its original quarantined, a
#     blocking row recorded - and the report is recomposed so the PUBLISHED
#     report states it; a tree that cannot be proved clean is replaced by an
#     incident-only notice. Every failure of that transaction is recorded
#     through `record_blocking_row` before the report is composed, so the
#     table, the printed tally and the exit status cannot disagree.
#   * Nothing is uploaded from where it was written. CI uploads target/publish
#     and nothing else, and `finalize_publication` builds that tree from sanitized
#     copies it has scanned - on every exit path, because an aborted run has
#     produced just as much to upload as a finished one.
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
  # The tree CI uploads, and the only one it uploads: sanitized copies of the
  # evidence, each one scanned before it is kept. Built by
  # `finalize_publication`; see the publication section below for why the raw
  # evidence is not what leaves the machine.
  PUBLISH_DIR="$TARGET_DIR/publish"
  # Where an artifact that carries a credential signature is MOVED to, so
  # that detecting one takes it out of every path CI publishes instead of
  # merely returning a non-zero status. Private, never uploaded.
  QUARANTINE_DIR="$TARGET_DIR/quarantine"
  PUBLICATION_STATUS_FILE="$PUBLISH_DIR/PUBLICATION-STATUS.txt"
  PUBLICATION_MANIFEST_FILE="$PUBLISH_DIR/MANIFEST.txt"
  LOG_DIR="$AUDIT_DIR/logs"
  SNAPSHOT_DIR="$AUDIT_DIR/snapshot"
  REPORT_FILE="$TARGET_DIR/gate-report.md"
  APPENDIX_FILE="$AUDIT_DIR/appendices.md"
  FRAMEWORK_ERROR_FILE="$AUDIT_DIR/framework-errors.txt"
  # Which commit, which run, and when - written before the first row and
  # copied into the published tree, so a reader can tell whether a report
  # describes the tree in front of them or one from an earlier day.
  RUN_IDENTITY_FILE="$AUDIT_DIR/run-identity.txt"
  # The exclusive per-checkout output lock; see `acquire_output_lock`. It is a
  # directory, and it lives beside the evidence it protects rather than in a
  # temporary directory, so it is per CHECKOUT - which is the scope that
  # matters, because it is the evidence files of this checkout that two runs
  # would interleave their writes into. `sbt clean` does not reach it: the
  # build's `cleanFiles` cover target/test-reports and target/parity-report,
  # not target/ itself.
  OUTPUT_LOCK_DIR="$TARGET_DIR/.gate-lock"
}

resolve_locations

COLLECT_MAIN="strata-collect/src/main/scala"
BASICS_MAIN="strata-basics/src/main/scala"
COLLECT_CLASSES="strata-collect/target/scala-2.13/classes"
BASICS_CLASSES="strata-basics/target/scala-2.13/classes"
COLLECT_TEST_CLASSES="strata-collect/target/scala-2.13/test-classes"
BASICS_TEST_CLASSES="strata-basics/target/scala-2.13/test-classes"

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

usage_error() {
  printf 'FATAL: %s\n\n' "$1" >&2
  printf 'Usage: %s                       run every gate and write the report\n' "$SCRIPT_NAME" >&2
  printf '       %s --verify-publication  re-check target/publish and exit\n' "$SCRIPT_NAME" >&2
  printf '       %s -h|--help             print the usage and exit\n\n' "$SCRIPT_NAME" >&2
  printf 'This script takes no options that change what it measures: it runs\n' >&2
  printf 'every gate, always. It accepts no argument at all, or exactly one of\n' >&2
  printf 'the two flags above - nothing else, and never a second argument, not\n' >&2
  printf 'even after a flag it does understand, because an argument it ignored\n' >&2
  printf 'would be an instruction it did not carry out.\n' >&2
}

# classify_invocation <arguments...> - the complete argument contract, in the
# one place that holds it.
#
# Prints the mode on stdout - `run`, `help` or `verify-publication` - and
# returns 0; reports the usage error on stderr and returns 1 for every other
# spelling and every other arity. It never exits and has no other effect, so
# it is safe to read through a command substitution, which is what `main`
# needs: `main` has to know whether this is the publication check BEFORE
# `init_run` takes the lock and empties anything, and a helper that exited
# from inside `$( ... )` would exit only the subshell and let the run
# continue.
#
# It exists because the mode used to be recognised in `main` on "${1:-}"
# alone while this function held the arity rule, so the two disagreed:
# `--verify-publication extra` matched the mode test, never reached the arity
# check, and exited 0 having silently discarded the surplus word - the one
# outcome the usage text above promises cannot happen. One decision, read by
# both callers, is what keeps the promise and the behaviour the same thing.
classify_invocation() {
  case "$#" in
    0)
      printf 'run\n'
      return 0
      ;;
    1)
      case "$1" in
        -h | --help)
          printf 'help\n'
          return 0
          ;;
        --verify-publication)
          printf 'verify-publication\n'
          return 0
          ;;
        *)
          usage_error "unknown argument \"$1\"."
          return 1
          ;;
      esac
      ;;
    *)
      usage_error "$# arguments given; this script accepts at most one - the help flag or --verify-publication - and never a second, whatever the first one is. Unexpected: $*"
      return 1
      ;;
  esac
}

# The side-effecting half of the contract, called by `init_run`: it turns the
# decision above into this process's behaviour. The help flag prints the usage
# and exits 0 here - before the output lock is taken, which is what lets `-h`
# answer while another acceptance run is in progress - and a usage error exits
# 2. `verify-publication` is intercepted by `main` before `init_run` is
# called, so reaching it here means a caller that sourced this file called
# `init_run` with that mode itself: it is rejected rather than run, because
# `init_run` is the function that would take the lock and empty the evidence
# trees the mode exists to leave untouched.
parse_arguments() {
  local mode
  mode="$(classify_invocation "$@")" || exit 2
  case "$mode" in
    help)
      usage
      exit 0
      ;;
    verify-publication)
      usage_error "--verify-publication runs no gate and must not be started through init_run; call $SCRIPT_NAME --verify-publication directly."
      exit 2
      ;;
    *)
      return 0
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
#
# Those checks are necessary and they are not sufficient, because a check made
# on a PATHNAME is a check on what the name meant at that instant. Between
# `[[ -L $path ]]` and the write that follows it, the name can be pointed
# somewhere else; `[[ -e && ! -d ]]` says nothing about a REGULAR file that is
# a second hard link to a file elsewhere, and truncating it truncates that
# file too; and a parent component validated one call ago can be renamed and
# replaced before the next one (CWE-367 check-to-use race, CWE-59). So the
# pathname checks below are a fast, readable first refusal, and the decision
# is made a second time by `fs_guard` - which opens every component no-follow
# relative to its parent's DESCRIPTOR, reads every property with `fstat` on
# the descriptor it is about to write through, and refuses a file with more
# than one link or an owner other than this run's. A name that changes after
# `fs_guard` has opened it changes nothing: there is no second lookup left to
# subvert.
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

# fs_guard <verb> <repository root> <arguments...>
#
# The descriptor-bound half of this framework, and the only code here that
# creates, empties or deletes anything under target/. It is written in python3
# because bash cannot open a path without following it, cannot read a
# descriptor's link count, owner or inode, and cannot write through a
# descriptor it has already validated - and those three are exactly what
# distinguishes a safe write into a git-ignored tree from a hope. python3 is
# in `preflight`'s required list and only its standard library is used, so
# this adds no dependency; it is invoked once per directory and once per
# truncation, and an interpreter start per evidence file is not a cost worth
# trading a class of filesystem race for.
#
# It prints its result on stdout and the reason for a refusal on stderr, and
# exits non-zero on refusal, so a caller reads it as
# `guard="$(fs_guard ... 2>&1)" || path_fatal "... ($guard)"` and never has to
# parse a status code. It is called BEFORE `install_checked_commands`, which is
# why it wraps no text utility and needs none.
fs_guard() {
  # The program is read from descriptor 3, not from stdin: two of the verbs
  # take the bytes they write ON stdin, and `python3 - ` would consume that
  # stream as the program itself. Argument indexing is unchanged - the verb is
  # still sys.argv[1].
  python3 /dev/fd/3 "$@" 3<<'PY'
"""fs_guard - the descriptor-bound half of this script's output framework.

Everything under $ROOT/target arrives from outside this run: target/ is
git-ignored, so a symbolic link, a hard link, a renamed parent or a planted
FIFO inside it is attacker-supplied input, and a check made on a PATHNAME is
undone by whatever happens between that check and the write it guards
(CWE-59 link following, CWE-367 check-to-use race, CWE-22 traversal). So
nothing here trusts a name twice: every component is opened with O_NOFOLLOW
relative to a descriptor for its parent, every property is read from the
DESCRIPTOR with fstat, and every mutation - mkdir, fchmod, ftruncate, unlink,
rmdir - is applied to that descriptor or relative to it. A rename or a relink
between two steps therefore cannot redirect a write, because there is no
second lookup of the name left to subvert.

Verbs, each with the repository root as its first argument so that this
program derives nothing from the environment:

  ensure-dir <root> <dir> <mode>     create/validate, print "dev:ino"
  verify-dir <root> <dir> <dev:ino>  re-open and confirm the same directory
  truncate   <root> <file>           empty one regular file, print "dev:ino"
  write      <root> <file>           replace it with stdin by O_EXCL + rename
  append     <root> <file>           append stdin through a checked descriptor
  verify-fd  <root> <file> <fd> <id> confirm an inherited append descriptor
  copy-tree  <root> <from> <to>      descriptor-bound recursive copy
  lock-dir   <root> <dir>            create the lock directory, or say who has it
  prune      <root> <dir>...         empty a directory, keeping the directory
  rmtree     <root> <dir>            delete one subtree below <root>/target
  stage      ...                     build the publication tree (see `stage`)
  quarantine <root> <dest> <path>... move artifacts out of every published path
  withdraw   <root> <file> <reason>  replace a staged copy with a marker

Exit 0 with the result on stdout, or exit 1 with the reason on stderr.
"""

import errno
import fcntl
import hashlib
import os
import re
import stat
import sys
import time
from xml.etree import ElementTree

DIR_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_CLOEXEC
NOFOLLOW_DIR_FLAGS = DIR_FLAGS | os.O_NOFOLLOW
# An evidence file is opened O_CREAT WITHOUT O_TRUNC, so nothing is emptied
# before fstat has proved what was opened, and O_NONBLOCK, so that a FIFO left
# in place of an evidence file makes this refuse rather than hang waiting for
# a reader that never comes.
FILE_FLAGS = os.O_WRONLY | os.O_CREAT | os.O_NOFOLLOW | os.O_NONBLOCK | os.O_CLOEXEC
EUID = os.geteuid()
GROUP_OTHER_WRITE = stat.S_IWGRP | stat.S_IWOTH
GROUP_OTHER_ALL = stat.S_IRWXG | stat.S_IRWXO
# Ancestors are brought into existence with the conventional directory mode:
# the mode the caller asks for describes the directory it named, not the tree
# above it, so ensuring a private directory never tightens target/ itself.
ANCESTOR_MODE = 0o755
# The one subtree of the checkout this program may delete from.
DELETABLE_ROOT = "target"
LINK_ERRNOS = (errno.ELOOP, errno.EMLINK)


def fail(reason):
    """Refuses the operation, naming what was refused and why."""
    sys.stderr.write(reason + "\n")
    raise SystemExit(1)


def relative_components(root, path):
    """The components of <path> below <root>, or a refusal."""
    if not root.startswith("/"):
        fail("the repository root %s is not an absolute path" % root)
    if not path.startswith("/"):
        fail("%s is not an absolute path" % path)
    prefix = root.rstrip("/") + "/"
    if not path.startswith(prefix):
        fail("%s is not below %s" % (path, root))
    components = path[len(prefix):].split("/")
    for component in components:
        if component in ("", ".", ".."):
            fail('%s contains the path element "%s"' % (path, component))
    return components


def open_root(root):
    """A descriptor for the checkout root, the one path taken on trust."""
    try:
        return os.open(root, DIR_FLAGS)
    except OSError as error:
        fail("the repository root %s cannot be opened as a directory (%s)"
             % (root, error))


def describe_obstacle(parent_fd, name):
    """Why a component could not be opened as a directory, for the message."""
    try:
        info = os.lstat(name, dir_fd=parent_fd)
    except OSError as error:
        return "could not be examined (%s)" % error
    if stat.S_ISLNK(info.st_mode):
        return "is a symbolic link"
    return "exists and is not a directory"


def open_or_make(parent_fd, name, shown, mode):
    """Opens one component of the path, creating it when `mode` is not None.

    Returns (descriptor, created). The descriptor is what every check and
    every change afterwards uses. `created` says whether THIS call made the
    directory, which is what decides between setting its mode outright and
    only tightening what was already there. mkdir's mode is masked by the
    umask, so the caller sets the final mode on the descriptor.
    """
    created = False
    for attempt in (1, 2):
        try:
            return os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=parent_fd), created
        except FileNotFoundError:
            if attempt == 2 or mode is None:
                fail("%s does not exist" % shown)
            try:
                os.mkdir(name, mode, dir_fd=parent_fd)
                created = True
            except FileExistsError:
                # Another process created it between the open and the mkdir.
                # The next open decides what it actually is; a symlink or a
                # file planted in that window is refused there.
                created = False
            except OSError as error:
                fail("%s could not be created (%s)" % (shown, error))
        except OSError as error:
            if error.errno in LINK_ERRNOS or error.errno == errno.ENOTDIR:
                # Which of the two it is decides what the operator has to fix,
                # and the kernel does not distinguish them here: O_NOFOLLOW
                # with O_DIRECTORY reports ENOTDIR for a symbolic link as well
                # as for a plain file. One lstat on the same descriptor-
                # relative name says which, and it is read for the MESSAGE
                # only - the refusal above is already decided.
                fail("%s %s" % (shown, describe_obstacle(parent_fd, name)))
            fail("%s could not be opened as a directory (%s)" % (shown, error))
    fail("%s could not be opened as a directory" % shown)


def set_directory_mode(fd, info, shown, mode, created):
    """Brings a directory's mode to what this run needs, never loosening it.

    A directory this run created gets exactly the mode asked for, keeping any
    set-group-id or sticky bit it inherited: neither grants write access, and
    a shared checkout relies on the group bit. A directory that was already
    there is only ever TIGHTENED - the group and other WRITE bits go, because
    a group- or world-writable evidence directory is an open invitation to
    plant the very links this program refuses, and when the caller asks for a
    private mode (no group or other bits at all) the remaining group and other
    bits go with them. No permission is ever ADDED to a directory this run
    did not create.
    """
    current = stat.S_IMODE(info.st_mode)
    if created:
        wanted = mode | (current & (stat.S_ISGID | stat.S_ISVTX))
    else:
        wanted = current & ~GROUP_OTHER_WRITE
        if not mode & GROUP_OTHER_ALL:
            wanted &= ~GROUP_OTHER_ALL
    if wanted != current:
        try:
            os.fchmod(fd, wanted)
        except OSError as error:
            fail("the mode of %s could not be set to %04o (%s)"
                 % (shown, wanted, error))


def walk(root, components, modes):
    """Descends the components from the root, returning the last descriptor.

    `modes[i]` is the mode component i is created with, or None to require it
    to exist already. Every component is opened no-follow from its parent's
    descriptor and every one is proved to be a directory this run's uid owns:
    a directory owned by somebody else below target/ is refused rather than
    written into, because its owner can replace anything inside it at will.
    """
    fd = open_root(root)
    shown = root.rstrip("/")
    try:
        for index, component in enumerate(components):
            shown = shown + "/" + component
            child, created = open_or_make(fd, component, shown, modes[index])
            os.close(fd)
            fd = child
            info = os.fstat(fd)
            if not stat.S_ISDIR(info.st_mode):
                fail("%s is not a directory" % shown)
            if info.st_uid != EUID:
                fail("%s is owned by uid %d, not by this run's uid %d"
                     % (shown, info.st_uid, EUID))
            if modes[index] is not None:
                set_directory_mode(fd, info, shown, modes[index], created)
    except BaseException:
        os.close(fd)
        raise
    return fd


def identity(fd):
    info = os.fstat(fd)
    return "%d:%d" % (info.st_dev, info.st_ino)


def ensure_dir(root, path, mode_text):
    try:
        mode = int(mode_text, 8)
    except ValueError:
        fail('"%s" is not an octal directory mode' % mode_text)
    if mode & ~0o777:
        fail('"%s" is not a plain permission mode' % mode_text)
    components = relative_components(root, path)
    modes = [ANCESTOR_MODE] * (len(components) - 1) + [mode]
    fd = walk(root, components, modes)
    try:
        sys.stdout.write(identity(fd) + "\n")
    finally:
        os.close(fd)


def verify_dir(root, path, expected):
    components = relative_components(root, path)
    fd = walk(root, components, [None] * len(components))
    try:
        found = identity(fd)
    finally:
        os.close(fd)
    if found != expected:
        fail("%s is no longer the directory this run validated "
             "(expected dev:ino %s, found %s)" % (path, expected, found))
    sys.stdout.write(found + "\n")


def truncate(root, path):
    components = relative_components(root, path)
    parents = components[:-1]
    name = components[-1]
    fd = walk(root, parents, [None] * len(parents))
    try:
        try:
            handle = os.open(name, FILE_FLAGS, 0o600, dir_fd=fd)
        except OSError as error:
            if error.errno in LINK_ERRNOS:
                fail("%s is a symbolic link" % path)
            if error.errno == errno.EISDIR:
                fail("%s is a directory" % path)
            if error.errno == errno.ENXIO:
                fail("%s is a pipe or a device, not a regular file" % path)
            fail("%s could not be opened for writing (%s)" % (path, error))
    finally:
        os.close(fd)
    try:
        info = os.fstat(handle)
        if not stat.S_ISREG(info.st_mode):
            fail("%s is not a regular file" % path)
        # The hard-link check is the one a pathname cannot make: a second name
        # for this inode, planted anywhere this uid can write, would have its
        # content emptied by the ftruncate below just as surely as the
        # evidence file would.
        if info.st_nlink != 1:
            fail("%s has %d hard links, so emptying it would empty every "
                 "other name for the same file" % (path, info.st_nlink))
        if info.st_uid != EUID:
            fail("%s is owned by uid %d, not by this run's uid %d"
                 % (path, info.st_uid, EUID))
        os.ftruncate(handle, 0)
        os.fchmod(handle, 0o600)
        # The identity of what was emptied, so the caller can bind a later
        # descriptor to THIS inode: `guarded_open_append` opens the same name
        # and refuses unless the descriptor it gets back is this file.
        identity_text = identity(handle)
    finally:
        os.close(handle)
    sys.stdout.write("%s\n" % identity_text)


def open_write_parent(root, path):
    """(parent descriptor, final component) for a file this program may write."""
    components = relative_components(root, path)
    parents = components[:-1]
    return walk(root, parents, [None] * len(parents)), components[-1]


def write_atomic(root, path):
    """Replaces <path> with stdin, atomically, without ever opening the target.

    The bytes go into a brand-new inode created O_EXCL inside the parent's
    DESCRIPTOR, are flushed to the device, and only then take the name by a
    rename relative to that same descriptor. Nothing about the object the name
    referred to before matters: it is not opened, not truncated and not
    followed, so a symbolic link, a hard link to a file elsewhere, a FIFO or a
    file owned by another account planted at that name cannot receive a single
    byte of what this writes - it is simply replaced. The rename is atomic, so
    a reader of the name sees either the whole previous file or the whole new
    one, never a half-written report.
    """
    payload = sys.stdin.buffer.read()
    parent, name = open_write_parent(root, path)
    temporary = ".%s.%d.tmp" % (name, os.getpid())
    try:
        try:
            handle = os.open(temporary,
                             os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW
                             | os.O_CLOEXEC, 0o600, dir_fd=parent)
        except OSError as error:
            fail("%s could not be staged for writing as %s (%s)"
                 % (path, temporary, error))
        written = 0
        try:
            try:
                while written < len(payload):
                    written += os.write(handle, payload[written:])
                os.fsync(handle)
                info = os.fstat(handle)
            finally:
                os.close(handle)
            try:
                os.rename(temporary, name, src_dir_fd=parent, dst_dir_fd=parent)
            except OSError as error:
                fail("%s could not be given its name (%s)" % (path, error))
        except BaseException:
            unlink_gone_is_fine(temporary, parent, path)
            raise
    finally:
        os.close(parent)
    sys.stdout.write("bytes=%d %d:%d\n" % (len(payload), info.st_dev, info.st_ino))


def rename_checked(root, source, destination):
    """Gives <source>'s inode the name <destination>, refusing a destination
    that is not a plain file.

    This is how the gate report is published, and it publishes the very inode
    that was scanned rather than a re-read copy of it, so nothing can change
    between the scan and the publication. `mv` cannot do this job: `mv -f file
    directory` does not replace the directory, it moves the file INSIDE it and
    reports success - so a directory planted at the report's name left the run
    announcing a report that was not at that name, which is the one thing the
    scan-before-rename order exists to prevent. Both names are resolved inside
    a single parent DESCRIPTOR opened without following links; the source must
    be a regular file with one link, so the bytes that were scanned cannot
    also be reachable under another name; the destination is refused unless it
    is absent or itself a regular file; and the rename is relative to that
    descriptor, so a parent swapped after the checks cannot redirect it.

    A rename never follows a link, so neither this nor `mv` could write
    through one - refusing a link here is about not consuming a name the run
    did not create, not about a victim outside the tree.
    """
    source_directory = os.path.dirname(source)
    if source_directory != os.path.dirname(destination):
        fail("%s and %s are not in one directory, which this rename requires"
             % (source, destination))
    parent, name = open_write_parent(root, destination)
    try:
        source_name = os.path.basename(source)
        try:
            info = os.lstat(source_name, dir_fd=parent)
        except OSError as error:
            fail("%s cannot be published (%s)" % (source, error))
        if not stat.S_ISREG(info.st_mode):
            fail("%s is not a regular file, so it is not published" % source)
        if info.st_nlink != 1:
            fail("%s has %d links, so it is not published"
                 % (source, info.st_nlink))
        try:
            existing = os.lstat(name, dir_fd=parent)
        except FileNotFoundError:
            existing = None
        except OSError as error:
            fail("%s could not be examined (%s)" % (destination, error))
        if existing is not None and not stat.S_ISREG(existing.st_mode):
            fail("%s exists and is not a regular file, so nothing is renamed "
                 "onto it" % destination)
        try:
            os.rename(source_name, name, src_dir_fd=parent, dst_dir_fd=parent)
        except OSError as error:
            fail("%s could not be renamed onto %s (%s)"
                 % (source, destination, error))
    finally:
        os.close(parent)
    sys.stdout.write("renamed %d:%d\n" % (info.st_dev, info.st_ino))


def append_checked(root, path):
    """Appends stdin to <path> through a descriptor it has proved is the file.

    O_APPEND|O_NOFOLLOW|O_CREAT, then `fstat` on the descriptor: a regular
    file, one link, this run's uid. The write happens through that descriptor,
    so the decision and the write are the same object - the name cannot be
    pointed elsewhere in between. Appending cannot use `write_atomic`'s
    replace-by-rename, because the point of an append is to keep what is
    already there.
    """
    payload = sys.stdin.buffer.read()
    parent, name = open_write_parent(root, path)
    try:
        try:
            handle = os.open(name,
                             os.O_WRONLY | os.O_CREAT | os.O_APPEND | os.O_NOFOLLOW
                             | os.O_NONBLOCK | os.O_CLOEXEC, 0o600, dir_fd=parent)
        except OSError as error:
            if error.errno in LINK_ERRNOS:
                fail("%s is a symbolic link" % path)
            if error.errno == errno.EISDIR:
                fail("%s is a directory" % path)
            if error.errno == errno.ENXIO:
                fail("%s is a pipe or a device, not a regular file" % path)
            fail("%s could not be opened for appending (%s)" % (path, error))
    finally:
        os.close(parent)
    try:
        info = os.fstat(handle)
        if not stat.S_ISREG(info.st_mode):
            fail("%s is not a regular file" % path)
        if info.st_nlink != 1:
            fail("%s has %d hard links, so appending to it appends to every other "
                 "name for the same file" % (path, info.st_nlink))
        if info.st_uid != EUID:
            fail("%s is owned by uid %d, not by this run's uid %d"
                 % (path, info.st_uid, EUID))
        written = 0
        while written < len(payload):
            written += os.write(handle, payload[written:])
    finally:
        os.close(handle)
    sys.stdout.write("bytes=%d\n" % len(payload))


def verify_fd(root, path, descriptor_text, expected):
    """Confirms an inherited descriptor is the append handle for <path>.

    The shell opens the evidence file once with `exec {fd}>>`, which is a
    pathname open and therefore the last moment at which a swap could still
    redirect it. This is the check that closes that moment: the descriptor -
    inherited by this process, so `fstat` describes exactly what the shell
    will write through - must be a regular, singly-linked file owned by this
    run, opened in append mode, and the SAME inode `truncate` validated when
    it created the file. Every write the shell makes afterwards goes to that
    inode whatever happens to the name, so this is checked once and holds for
    the life of the descriptor.
    """
    relative_components(root, path)
    try:
        descriptor = int(descriptor_text)
    except ValueError:
        fail('"%s" is not a file descriptor number' % descriptor_text)
    try:
        info = os.fstat(descriptor)
    except OSError as error:
        fail("descriptor %d for %s cannot be examined (%s); it was not inherited"
             % (descriptor, path, error))
    if not stat.S_ISREG(info.st_mode):
        fail("descriptor %d for %s is not a regular file" % (descriptor, path))
    if info.st_nlink != 1:
        fail("descriptor %d for %s has %d hard links"
             % (descriptor, path, info.st_nlink))
    if info.st_uid != EUID:
        fail("descriptor %d for %s is owned by uid %d, not by this run's uid %d"
             % (descriptor, path, info.st_uid, EUID))
    actual = "%d:%d" % (info.st_dev, info.st_ino)
    if expected and actual != expected:
        fail("descriptor %d is %s, not the %s this run created for %s, so the name "
             "was replaced between creating the file and opening it"
             % (descriptor, actual, expected, path))
    try:
        flags = fcntl.fcntl(descriptor, fcntl.F_GETFL)
    except OSError as error:
        fail("the mode of descriptor %d for %s cannot be read (%s)"
             % (descriptor, path, error))
    if not flags & os.O_APPEND:
        fail("descriptor %d for %s is not in append mode, so a write through it "
             "could overwrite what is already recorded" % (descriptor, path))
    sys.stdout.write("%s\n" % actual)


def unlink_gone_is_fine(name, dir_fd, shown):
    """Unlinks one name; already gone is the outcome asked for, not a failure."""
    try:
        os.unlink(name, dir_fd=dir_fd)
    except FileNotFoundError:
        return
    except OSError as error:
        fail("%s could not be deleted (%s)" % (shown, error))


def remove_tree(parent_fd, name, shown, device):
    """Deletes one subtree, descending only into real directories."""
    try:
        fd = os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=parent_fd)
    except FileNotFoundError:
        return
    except OSError as error:
        if error.errno in LINK_ERRNOS or error.errno == errno.ENOTDIR:
            # A symbolic link or a plain file: unlinked, never descended, so
            # nothing outside this tree is touched by deleting it.
            unlink_gone_is_fine(name, parent_fd, shown)
            return
        fail("%s could not be opened for deletion (%s)" % (shown, error))
    try:
        info = os.fstat(fd)
        if info.st_dev != device:
            fail("%s is on a different filesystem (device %s) than the output "
                 "tree (device %s), so it is not deleted"
                 % (shown, info.st_dev, device))
        entries = []
        with os.scandir(fd) as scan:
            for entry in scan:
                entries.append((entry.name, entry.is_dir(follow_symlinks=False)))
        for child_name, is_directory in entries:
            if is_directory:
                remove_tree(fd, child_name, shown + "/" + child_name, device)
            else:
                unlink_gone_is_fine(child_name, fd, shown + "/" + child_name)
    finally:
        os.close(fd)
    try:
        os.rmdir(name, dir_fd=parent_fd)
    except FileNotFoundError:
        return
    except OSError as error:
        fail("%s could not be removed (%s)" % (shown, error))


def rmtree(root, path):
    components = relative_components(root, path)
    if components[0] != DELETABLE_ROOT:
        fail("%s is not below %s/%s, so this run will not delete it"
             % (path, root, DELETABLE_ROOT))
    if len(components) < 2:
        fail("%s is the output tree itself, which is never deleted" % path)
    parents = components[:-1]
    fd = walk(root, parents, [None] * len(parents))
    try:
        remove_tree(fd, components[-1], path, os.fstat(fd).st_dev)
    finally:
        os.close(fd)
    sys.stdout.write("ok\n")


# --------------------------------------------------------------------------
# Publication: the copies that leave the machine.
#
# CI uploads ONE tree, and this is what builds it. Nothing is uploaded from
# where it was written: every artifact is copied into the publication tree
# first, and on the way it is
#
#   * proved to be what it claims - opened no-follow, then a regular,
#     singly-linked file owned by this run's uid, read from the DESCRIPTOR -
#     so a link or a file swapped into the evidence tree after the row that
#     wrote it cannot have its content published (CWE-59, CWE-367);
#   * proved to belong to THIS run, by a modification time at or after the
#     run's start: a report from an earlier run is withheld and named rather
#     than published as current evidence (CWE-345);
#   * SANITIZED - the JUnit `<properties>` block and the `hostname` attribute
#     go, and every path in the redaction table is replaced by a placeholder,
#     so the published copy carries no runner hostname, account, home
#     directory, checkout path, temporary directory or toolchain location
#     (CWE-200).
#
# The bytes are transformed through `str` with `surrogateescape`, which
# round-trips any byte sequence exactly, so a binary artifact is copied
# faithfully rather than skipped or corrupted.
#
# What is NOT done here is the secret scan: the caller scans the tree this
# produces, because scanning the copies means scanning the exact bytes that
# will be uploaded, and because one scanner implementation - the one whose
# rules and allowlists are auditable in one place - is better than two.
# --------------------------------------------------------------------------

# A file written in the same second the run started is this run's: filesystem
# timestamps and the shell's `date` do not share a sub-second clock.
STALE_GRACE_SECONDS = 2
# The same allowance in the other direction. A timestamp later than now was
# set deliberately - no process writing a file honestly produces one - and an
# artifact carrying it is withheld rather than published.
FUTURE_SKEW_SECONDS = 2
# The JUnit XML `<properties>` element, which ScalaTest's `-u` reporter fills
# from `System.getProperties()` - every JVM property of the build machine,
# `user.dir`, `user.home`, `java.io.tmpdir` and the JDK's own library path
# among them. The reporter has no option to suppress it, so the published
# copy has it removed instead. Non-greedy, so two suites in one file each
# lose their own block.
PROPERTIES_ELEMENT = re.compile(r"[ \t]*<properties>.*?</properties>[ \t]*\n?", re.S)
# The suite's `hostname` attribute: the build machine's name, in a document
# whose value is the test counts.
HOSTNAME_ATTRIBUTE = re.compile(r'(hostname=")[^"]*(")')


def read_checked(root, path, what):
    """The bytes of a file whose content drives a decision, read no-follow.

    An ordinary `open` follows a symbolic link and reads whatever it points
    at. For a file this program merely copies that would be caught by the
    staging checks; for a file that TELLS this program what to redact or what
    to verify, a substituted file silently changes the decision - an emptied
    redaction table means nothing is redacted (CWE-59 leading to CWE-200). So
    the parent is walked descriptor by descriptor and the file is opened
    no-follow and proved, on its descriptor, to be a regular file this run
    owns before a byte of it is believed.
    """
    parent, name = open_write_parent(root, path)
    try:
        handle, info = open_regular_checked(parent, name)
    finally:
        os.close(parent)
    if handle is None:
        fail("%s %s (%s)" % (path, info, what))
    try:
        with os.fdopen(handle, "rb") as reader:
            return reader.read()
    except OSError as error:
        fail("%s could not be read (%s)" % (path, error))


def read_redactions(root, path):
    """The redaction table: literal needle, TAB, replacement, longest first.

    Literal rather than regular expressions, because the needles are paths
    that come from the environment and would otherwise have to be escaped;
    longest first, so `$HOME/.cache/coursier` is replaced before `$HOME` and
    the more specific placeholder wins.
    """
    pairs = []
    payload = read_checked(root, path, "the redaction table")
    for line in payload.decode("utf-8", "surrogateescape").split("\n"):
        line = line.rstrip("\r")
        if not line or line.startswith("#"):
            continue
        needle, _, replacement = line.partition("\t")
        if needle:
            pairs.append((needle, replacement))
    if not pairs:
        fail("the redaction table %s named nothing to redact, so no copy would be "
             "sanitized; it is a defect rather than an empty rule set" % path)
    pairs.sort(key=lambda pair: len(pair[0]), reverse=True)
    return pairs


def well_formed_xml(text):
    """(True, None) if this parses as XML, else (False, the parser's reason).

    Redaction is a literal text substitution, and a substitution inside an XML
    document can only be published if the result is still a document. A
    placeholder carrying `<` or `>` - which is what `<checkout>` was - lands
    inside attribute values such as name="fails at /home/x/repo" and produces
    name="fails at <checkout>", which is not well-formed; `store_test_results`
    then rejects the whole suite file, and it does so precisely on a FAILING
    run, where the paths appear in failure text rather than only in the
    stripped properties block. The placeholders no longer contain markup
    characters, and this is the check that proves it for every document
    actually published rather than trusting that they never will.
    """
    try:
        ElementTree.fromstring(text)
    except ElementTree.ParseError as error:
        return False, str(error)
    except ValueError as error:
        return False, str(error)
    return True, None


def sanitize(name, text, redactions):
    """Returns the publishable text and the notes describing what was removed."""
    notes = []
    if name.endswith(".xml") and "<testsuite" in text:
        text, count = PROPERTIES_ELEMENT.subn("", text)
        if count:
            notes.append("properties-stripped=%d" % count)
        text, count = HOSTNAME_ATTRIBUTE.subn(r"\1redacted\2", text)
        if count:
            notes.append("hostname-redacted=%d" % count)
    redacted = 0
    for needle, replacement in redactions:
        occurrences = text.count(needle)
        if occurrences:
            text = text.replace(needle, replacement)
            redacted += occurrences
    if redacted:
        notes.append("paths-redacted=%d" % redacted)
    if name.endswith(".xml"):
        parsed, reason = well_formed_xml(text)
        if not parsed:
            # Withheld, not published unredacted and not published broken:
            # either would defeat one of the two things this pass is for.
            return None, "UNSAFE:the sanitized XML would not parse (%s)" % reason
        notes.append("xml-parsed=ok")
    return text, notes


class Staging(object):
    """The state of one staging pass, so the verbs stay free of globals."""

    def __init__(self, epoch, redactions):
        self.epoch = epoch
        # The present moment, read once so that every artifact of one pass is
        # judged against the same instant.
        self.now = time.time()
        self.redactions = redactions
        self.rows = []
        self.staged = 0
        self.bytes = 0
        self.sanitized = 0
        self.stale = 0
        self.unsafe = 0
        self.missing = 0

    def withhold(self, source_label, note):
        """Records an artifact that is NOT published, and why."""
        if note.startswith("STALE"):
            self.stale += 1
        elif note.startswith("MISSING"):
            self.missing += 1
        else:
            self.unsafe += 1
        self.rows.append("-\t%s\t0\t-\t%s" % (source_label, note))

    def keep(self, staged_label, source_label, size, digest, notes):
        self.staged += 1
        self.bytes += size
        if notes:
            self.sanitized += 1
        self.rows.append("%s\t%s\t%d\t%s\t%s"
                         % (staged_label, source_label, size, digest,
                            ",".join(notes) if notes else "verbatim"))

    def failed(self):
        return self.stale or self.unsafe or self.missing


def open_regular_checked(parent_fd, name):
    """(descriptor, stat) for a regular, singly-linked, own file, or (None, note)."""
    try:
        fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK | os.O_CLOEXEC,
                     dir_fd=parent_fd)
    except OSError as error:
        if error.errno in LINK_ERRNOS:
            return None, "UNSAFE:is a symbolic link, so its target is not published"
        return None, "UNSAFE:could not be opened (%s)" % error
    info = os.fstat(fd)
    if not stat.S_ISREG(info.st_mode):
        os.close(fd)
        return None, "UNSAFE:not a regular file"
    if info.st_nlink != 1:
        os.close(fd)
        return None, ("UNSAFE:has %d hard links, so it is not the only name for this "
                      "content" % info.st_nlink)
    if info.st_uid != EUID:
        os.close(fd)
        return None, "UNSAFE:owned by uid %d, not by this run's uid %d" % (info.st_uid, EUID)
    return fd, info


def stage_one_file(state, source_fd, name, dest_fd, dest_name, source_label, staged_label):
    fd, info = open_regular_checked(source_fd, name)
    if fd is None:
        state.withhold(source_label, info)
        return
    # Provenance is established by emptying the evidence trees before the
    # first row (see `prune`), so every artifact found here was produced
    # during this run. These two checks are the second, independent statement
    # of the same fact, and they are stated about the DESCRIPTOR: a timestamp
    # before the run started cannot be this run's work, and one in the future
    # is not a timestamp any process on this host wrote honestly - both are
    # withheld and named rather than published as current evidence.
    if info.st_mtime < state.epoch - STALE_GRACE_SECONDS:
        os.close(fd)
        state.withhold(source_label,
                       "STALE:written at %d, before this run started at %d, so it is not "
                       "this run's evidence" % (int(info.st_mtime), int(state.epoch)))
        return
    if info.st_mtime > state.now + FUTURE_SKEW_SECONDS:
        os.close(fd)
        state.withhold(source_label,
                       "STALE:dated %d, which is after the present moment %d, so its "
                       "timestamp was set rather than written"
                       % (int(info.st_mtime), int(state.now)))
        return
    try:
        with os.fdopen(fd, "rb") as handle:
            payload = handle.read()
    except OSError as error:
        state.withhold(source_label, "UNSAFE:could not be read (%s)" % error)
        return
    text, notes = sanitize(dest_name, payload.decode("utf-8", "surrogateescape"),
                           state.redactions)
    if text is None:
        state.withhold(source_label, notes)
        return
    output = text.encode("utf-8", "surrogateescape")
    # The destination is unlinked first, through the destination directory's
    # descriptor, and then created O_EXCL. Both halves matter: the unlink makes
    # a second staging pass possible - the finalization transaction restages
    # the report after a finding has been acted on - and the O_EXCL means the
    # bytes still go into a new inode rather than into whatever object was
    # sitting at that name.
    unlink_gone_is_fine(dest_name, dest_fd, staged_label)
    try:
        handle = os.open(dest_name,
                         os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW | os.O_CLOEXEC,
                         0o600, dir_fd=dest_fd)
    except OSError as error:
        state.withhold(source_label, "UNSAFE:the published copy could not be created (%s)"
                       % error)
        return
    try:
        with os.fdopen(handle, "wb") as sink:
            sink.write(output)
    except OSError as error:
        state.withhold(source_label, "UNSAFE:the published copy could not be written (%s)"
                       % error)
        return
    # The digest of the bytes that were PUBLISHED, not of the source: it is
    # the published copy the uploader is handed, so it is the published copy
    # the manifest lets it re-check. The window between this scan-approved
    # copy and the upload belongs to a different process, and a digest is what
    # carries the approval across it (CWE-345).
    state.keep(staged_label, source_label, len(output),
               hashlib.sha256(output).hexdigest(), notes)


def stage_one_directory(state, source_fd, dest_fd, source_label, staged_label):
    """Copies one directory's contents, descending only into real directories."""
    entries = []
    with os.scandir(source_fd) as scan:
        for entry in scan:
            entries.append((entry.name, entry.is_symlink(),
                            entry.is_dir(follow_symlinks=False)))
    for name, is_link, is_directory in sorted(entries):
        child_source = source_label + "/" + name
        child_staged = staged_label + "/" + name
        if is_link:
            state.withhold(child_source,
                           "UNSAFE:is a symbolic link, so its target is not published")
            continue
        if is_directory:
            try:
                child_fd = os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=source_fd)
            except OSError as error:
                state.withhold(child_source, "UNSAFE:could not be opened (%s)" % error)
                continue
            try:
                try:
                    os.mkdir(name, 0o700, dir_fd=dest_fd)
                except FileExistsError:
                    pass
                child_dest = os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=dest_fd)
            except OSError as error:
                os.close(child_fd)
                state.withhold(child_source,
                               "UNSAFE:its published directory could not be created (%s)"
                               % error)
                continue
            try:
                stage_one_directory(state, child_fd, child_dest, child_source, child_staged)
            finally:
                os.close(child_fd)
                os.close(child_dest)
            continue
        stage_one_file(state, source_fd, name, dest_fd, name, child_source, child_staged)


def stage(root, publish_dir, epoch_text, redactions_path, manifest_path, manifest_mode,
          *specs):
    """Builds the publication tree from `source=destination` specifications."""
    try:
        epoch = float(epoch_text)
    except ValueError:
        fail('"%s" is not an epoch second' % epoch_text)
    if manifest_mode not in ("truncate", "append"):
        fail('"%s" is neither truncate nor append' % manifest_mode)
    state = Staging(epoch, read_redactions(root, redactions_path))
    publish_components = relative_components(root, publish_dir)

    for spec in specs:
        source_rel, separator, dest_rel = spec.partition("=")
        if not separator or not source_rel or not dest_rel:
            fail('"%s" is not a source=destination specification' % spec)
        source_abs = root.rstrip("/") + "/" + source_rel
        source_components = relative_components(root, source_abs)
        source_parent = walk(root, source_components[:-1],
                             [None] * (len(source_components) - 1))
        try:
            name = source_components[-1]
            try:
                info = os.lstat(name, dir_fd=source_parent)
            except FileNotFoundError:
                state.withhold(source_rel, "MISSING:this run produced no such artifact")
                continue
            if stat.S_ISLNK(info.st_mode):
                state.withhold(source_rel,
                               "UNSAFE:is a symbolic link, so its target is not published")
                continue
            if stat.S_ISDIR(info.st_mode):
                source_fd = os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=source_parent)
                try:
                    dest_fd = walk(root, publish_components + dest_rel.split("/"),
                                   [None] * len(publish_components)
                                   + [0o700] * len(dest_rel.split("/")))
                    try:
                        stage_one_directory(state, source_fd, dest_fd, source_rel, dest_rel)
                    finally:
                        os.close(dest_fd)
                finally:
                    os.close(source_fd)
                continue
            dest_components = publish_components + dest_rel.split("/")
            dest_parent = walk(root, dest_components[:-1],
                               [None] * len(publish_components)
                               + [0o700] * (len(dest_components) - len(publish_components) - 1))
            try:
                stage_one_file(state, source_parent, name, dest_parent,
                               dest_components[-1], source_rel, dest_rel)
            finally:
                os.close(dest_parent)
        finally:
            os.close(source_parent)

    # The manifest is written through a descriptor for the publication
    # directory, like everything else this program writes: it is the record
    # the uploader re-checks the tree against, so a link planted at its name
    # must not be able to receive it or to redirect it.
    rows = list(state.rows)
    if manifest_mode == "truncate":
        rows.insert(0, "# staged-path\tsource-path\tpublished-bytes\tsha256\tnotes")
    payload = "".join(row + "\n" for row in rows).encode("utf-8", "surrogateescape")
    manifest_parent, manifest_name = open_write_parent(root, manifest_path)
    try:
        flags = os.O_WRONLY | os.O_CREAT | os.O_NOFOLLOW | os.O_CLOEXEC
        flags |= os.O_APPEND if manifest_mode == "append" else os.O_TRUNC
        try:
            handle = os.open(manifest_name, flags, 0o600, dir_fd=manifest_parent)
        except OSError as error:
            fail("the publication manifest %s could not be opened (%s)"
                 % (manifest_path, error))
    finally:
        os.close(manifest_parent)
    try:
        info = os.fstat(handle)
        if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1 or info.st_uid != EUID:
            fail("the publication manifest %s is not a regular, singly-linked file "
                 "owned by this run" % manifest_path)
        written = 0
        while written < len(payload):
            written += os.write(handle, payload[written:])
    finally:
        os.close(handle)

    sys.stdout.write("staged=%d bytes=%d sanitized=%d stale=%d unsafe=%d missing=%d\n"
                     % (state.staged, state.bytes, state.sanitized, state.stale,
                        state.unsafe, state.missing))
    if state.failed():
        raise SystemExit(1)


def quarantine(root, destination, *paths):
    """MOVES artifacts out of every published path, into a private directory.

    This is what makes a detection act on the artifact instead of only on the
    process status: a file that carries a credential signature stops being
    somewhere a later run, a later step or an operator can upload. The
    destination is 0700 and is never staged, and the move is a rename between
    two directories of the same output tree, so the content never passes
    through a third path.
    """
    destination_components = relative_components(root, destination)
    moved = 0
    for path_rel in paths:
        source_abs = root.rstrip("/") + "/" + path_rel
        source_components = relative_components(root, source_abs)
        source_parent = walk(root, source_components[:-1],
                             [None] * (len(source_components) - 1))
        try:
            flat = path_rel.replace("/", "_")
            dest_parent = walk(root, destination_components,
                               [0o700] * len(destination_components))
            try:
                os.rename(source_components[-1], flat,
                          src_dir_fd=source_parent, dst_dir_fd=dest_parent)
                moved += 1
            except FileNotFoundError:
                pass
            except OSError as error:
                fail("%s could not be quarantined into %s (%s)"
                     % (path_rel, destination, error))
            finally:
                os.close(dest_parent)
        finally:
            os.close(source_parent)
    sys.stdout.write("quarantined=%d\n" % moved)


def withdraw(root, path, reason):
    """Removes one staged copy and leaves a marker in its place.

    The marker is what keeps a withdrawn artifact from looking like one that
    was never produced: it names the file and the reason, and deliberately
    not the value that caused it.
    """
    components = relative_components(root, path)
    parent = walk(root, components[:-1], [None] * (len(components) - 1))
    try:
        unlink_gone_is_fine(components[-1], parent, path)
        marker = components[-1] + ".WITHHELD.txt"
        try:
            handle = os.open(marker,
                             os.O_WRONLY | os.O_CREAT | os.O_TRUNC | os.O_NOFOLLOW
                             | os.O_CLOEXEC,
                             0o600, dir_fd=parent)
        except OSError as error:
            fail("the marker for %s could not be created (%s)" % (path, error))
        with os.fdopen(handle, "w", encoding="utf-8") as sink:
            sink.write("This artifact was WITHHELD from publication.\n"
                       "reason: %s\n"
                       "The original has been quarantined outside every published path "
                       "and its content is not reproduced here.\n" % reason)
    finally:
        os.close(parent)
    sys.stdout.write("withdrawn=1\n")


def prune(root, *directories):
    """Empties each directory, keeping the directory itself.

    This is how a run establishes that its evidence trees hold its OWN
    evidence and nothing else. The alternative - deciding file by file whether
    something looks recent enough to belong to this run - cannot be made
    sound: a modification time is metadata any process with write access can
    set to any value, so "newer than the run started" is a guess about
    provenance rather than a fact about it (CWE-345). Emptying the tree before
    the first row runs needs no guess: everything found in it afterwards was
    put there during this run.

    The directory inode is preserved rather than removed and recreated, so the
    dev:ino this run validated and re-checks at every handover stays valid.
    Each entry is removed through `remove_tree`, which descends only into real
    directories and unlinks a symbolic link instead of following it.
    """
    removed = 0
    for directory in directories:
        components = relative_components(root, directory)
        if components[0] != DELETABLE_ROOT:
            fail("%s is not below %s/%s" % (directory, root, DELETABLE_ROOT))
        if len(components) < 2:
            fail("%s is the output tree itself, which is never emptied" % directory)
        parent = walk(root, components[:-1], [None] * (len(components) - 1))
        try:
            try:
                fd = os.open(components[-1], NOFOLLOW_DIR_FLAGS, dir_fd=parent)
            except FileNotFoundError:
                continue
            except OSError as error:
                if error.errno in LINK_ERRNOS or error.errno == errno.ENOTDIR:
                    fail("%s is a symbolic link or not a directory" % directory)
                fail("%s could not be opened (%s)" % (directory, error))
            try:
                device = os.fstat(fd).st_dev
                entries = []
                with os.scandir(fd) as scan:
                    for entry in scan:
                        entries.append((entry.name,
                                        entry.is_dir(follow_symlinks=False)))
                for child, is_directory in sorted(entries):
                    shown = directory + "/" + child
                    if is_directory:
                        remove_tree(fd, child, shown, device)
                    else:
                        unlink_gone_is_fine(child, fd, shown)
                    removed += 1
            finally:
                os.close(fd)
        finally:
            os.close(parent)
    sys.stdout.write("removed=%d\n" % removed)


def copy_tree(root, source, destination):
    """Copies a directory's contents descriptor-bound, for snapshots.

    `cp -a` resolves every name itself, which in a git-ignored tree means
    following whatever link or replaced parent it finds (CWE-59). This reads
    each file from a descriptor it has proved is a regular, singly-linked file
    owned by this run, and writes each copy into a fresh O_EXCL inode inside a
    descriptor for the destination directory. A symbolic link in the source is
    refused rather than dereferenced, and the destination is never opened by
    name, so nothing outside the tree can receive a byte.
    """
    copied = 0
    refused = []

    def copy_into(source_fd, dest_fd, shown):
        nonlocal copied
        entries = []
        with os.scandir(source_fd) as scan:
            for entry in scan:
                entries.append((entry.name, entry.is_symlink(),
                                entry.is_dir(follow_symlinks=False)))
        for name, is_link, is_directory in sorted(entries):
            child = shown + "/" + name
            if is_link:
                refused.append("%s is a symbolic link" % child)
                continue
            if is_directory:
                child_source = os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=source_fd)
                try:
                    try:
                        os.mkdir(name, 0o700, dir_fd=dest_fd)
                    except FileExistsError:
                        pass
                    child_dest = os.open(name, NOFOLLOW_DIR_FLAGS, dir_fd=dest_fd)
                    try:
                        copy_into(child_source, child_dest, child)
                    finally:
                        os.close(child_dest)
                finally:
                    os.close(child_source)
                continue
            handle, info = open_regular_checked(source_fd, name)
            if handle is None:
                refused.append("%s %s" % (child, info))
                continue
            try:
                with os.fdopen(handle, "rb") as reader:
                    payload = reader.read()
            except OSError as error:
                refused.append("%s could not be read (%s)" % (child, error))
                continue
            unlink_gone_is_fine(name, dest_fd, child)
            try:
                sink = os.open(name,
                               os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW
                               | os.O_CLOEXEC, 0o600, dir_fd=dest_fd)
            except OSError as error:
                refused.append("%s could not be created in the destination (%s)"
                               % (child, error))
                continue
            try:
                with os.fdopen(sink, "wb") as writer:
                    writer.write(payload)
            except OSError as error:
                refused.append("%s could not be written (%s)" % (child, error))
                continue
            copied += 1

    source_components = relative_components(root, source)
    dest_components = relative_components(root, destination)
    source_parent = walk(root, source_components[:-1],
                         [None] * (len(source_components) - 1))
    try:
        try:
            source_fd = os.open(source_components[-1], NOFOLLOW_DIR_FLAGS,
                                dir_fd=source_parent)
        except FileNotFoundError:
            fail("%s does not exist, so there is nothing to copy" % source)
        except OSError as error:
            fail("%s could not be opened (%s)" % (source, error))
    finally:
        os.close(source_parent)
    try:
        dest_fd = walk(root, dest_components,
                       [None] * (len(dest_components) - 1) + [0o700])
        try:
            copy_into(source_fd, dest_fd, source)
        finally:
            os.close(dest_fd)
    finally:
        os.close(source_fd)
    if refused:
        fail("%d file(s) could not be copied safely: %s"
             % (len(refused), "; ".join(refused)))
    sys.stdout.write("copied=%d\n" % copied)


def lock_dir(root, path):
    """Creates the output lock directory relative to its parent's descriptor.

    `mkdir` on a pathname would create the lock THROUGH a symbolic link left
    in target/, putting this run's lock - and the exclusion every other run
    depends on - outside the checkout. The mkdir here is relative to a
    descriptor for target/ that was opened no-follow, and its success is the
    atomic act that takes the lock. "held" on stdout means another run has it.
    """
    components = relative_components(root, path)
    if components[0] != DELETABLE_ROOT or len(components) != 2:
        fail("%s is not a direct child of %s/%s" % (path, root, DELETABLE_ROOT))
    parent = walk(root, components[:-1], [None])
    try:
        try:
            os.mkdir(components[-1], 0o700, dir_fd=parent)
        except FileExistsError:
            sys.stdout.write("held\n")
            return
        except OSError as error:
            fail("the output lock %s could not be created (%s)" % (path, error))
        try:
            fd = os.open(components[-1], NOFOLLOW_DIR_FLAGS, dir_fd=parent)
        except OSError as error:
            fail("the output lock %s was created but cannot be opened (%s)"
                 % (path, error))
        try:
            os.fchmod(fd, 0o700)
            sys.stdout.write("acquired %s\n" % identity(fd))
        finally:
            os.close(fd)
    finally:
        os.close(parent)


VERBS = {
    "ensure-dir": (ensure_dir, 3),
    "verify-dir": (verify_dir, 3),
    "truncate": (truncate, 2),
    "write": (write_atomic, 2),
    "append": (append_checked, 2),
    "rename": (rename_checked, 3),
    "verify-fd": (verify_fd, 4),
    "copy-tree": (copy_tree, 3),
    "lock-dir": (lock_dir, 2),
    "rmtree": (rmtree, 2),
    # Variadic: every evidence root the run empties before its first row.
    "prune": (prune, -2),
    # Variadic verbs, which is what the negative arity means: the publication
    # step names every artifact it stages, quarantines or sweeps.
    "stage": (stage, -7),
    "quarantine": (quarantine, -2),
    "withdraw": (withdraw, 3),
}

if len(sys.argv) < 2 or sys.argv[1] not in VERBS:
    fail("fs_guard: unknown verb %r; expected one of %s"
         % (sys.argv[1:2], ", ".join(sorted(VERBS))))
action, arity = VERBS[sys.argv[1]]
arguments = sys.argv[2:]
# A negative arity is a MINIMUM: the variadic verbs take a fixed head followed
# by one or more artifacts, and a verb invoked with nothing to act on is a
# defect in the caller rather than a no-op to be tolerated.
if arity < 0:
    if len(arguments) < -arity:
        fail("fs_guard %s expects at least %d argument(s), got %d"
             % (sys.argv[1], -arity, len(arguments)))
elif len(arguments) != arity:
    fail("fs_guard %s expects %d argument(s), got %d"
         % (sys.argv[1], arity, len(arguments)))
action(*arguments)
PY
}

# ensure_output_dir <absolute directory below $ROOT> [octal mode]
#
# Creates it if needed, refusing a symlinked or non-directory component on the
# way, and confirms the canonical result is inside the canonical repository
# root. Returns 1 - it never exits - so a caller inside a gate can record the
# failure as that row's verdict.
#
# The mode is the one the directory NAMED is to have, and defaults to 0755 -
# the mode of the directories this script shares with sbt and the forked test
# JVMs. The private evidence directories are ensured with 0700 by
# `init_output_tree`. An existing directory is only ever tightened, never
# loosened: see `set_directory_mode` inside `fs_guard`. Ancestors are created
# 0755 whatever the mode argument says, so ensuring a private directory
# cannot make target/ itself unreachable to the build.
#
# On success ENSURED_DIR_ID carries the dev:ino `fs_guard` validated, which is
# what `verify_output_tree_identity` later re-checks the directory against.
ENSURED_DIR_ID=""
ensure_output_dir() {
  local dir="$1"
  local mode="${2:-0755}"

  ENSURED_DIR_ID=""
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

  # The first refusal, by pathname: cheap, readable, and it names the problem
  # in the terms an operator sees in `ls`. It is deliberately NOT the one the
  # writes rely on - the loop below creates nothing, because creating a
  # directory by name is the step that can be redirected between components.
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
  done

  # The decision, by descriptor: every component is re-opened no-follow from
  # its parent's descriptor, created there if it is absent, proved to be a
  # directory this run's uid owns, and tightened if it was left group- or
  # world-writable. Whatever the names meant during the loop above, this is
  # what is actually written into.
  local guard
  if ! guard="$(fs_guard ensure-dir "$ROOT" "$dir" "$mode" 2>&1)"; then
    path_fatal "$dir (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  ENSURED_DIR_ID="$guard"

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

# copy_through_shell <from> <to> - a copy whose transfer needs no external
# tool, used only where the report must exist and `mv` has just proved
# unavailable. The read-and-write loop is builtins alone; the destination is
# emptied through `safe_truncate` like every other file this framework
# creates, because a fallback copy is still a write into the git-ignored tree
# and `: >"$to"` would follow a link or truncate a hard-linked file there just
# as readily as anywhere else. If the destination cannot be emptied safely
# this returns 1 and the caller reports that the copy was not made.
copy_through_shell() {
  local from="$1"
  local to="$2"

  # The read loop is builtins alone; the destination is written by
  # `guarded_write`, which replaces the name with a new inode instead of
  # opening whatever is at it. `: >"$to"` followed by appends - what this used
  # to do - decided what it was writing to and then looked the name up again
  # for every line.
  {
    local line
    while IFS= read -r line || [[ -n "$line" ]]; do
      printf '%s\n' "$line"
    done <"$from"
  } | guarded_write "$to"
}

# safe_truncate <absolute file below $ROOT> - the only way this framework
# empties or creates a file, so that no write can be redirected by a link.
#
# `: >"$file"` was not that: it follows a symbolic link, and it truncates a
# REGULAR file that is a second hard link to something else this uid can write
# - a link a previous process could have planted in the git-ignored target/
# tree, whose real target `[[ -e ]]` and `[[ -L ]]` cannot see. So the file is
# opened through `fs_guard` instead: O_CREAT without O_TRUNC and O_NOFOLLOW,
# then `fstat` on the DESCRIPTOR, then a refusal unless it is a regular file
# with exactly one link owned by this run's uid, and only then `ftruncate`.
# Nothing is emptied before it is known what would be emptied. The file is
# left mode 0600, which is also what keeps the evidence out of other accounts'
# reach while it is being written.
# On success TRUNCATED_FILE_ID carries the dev:ino of the file that was
# emptied, which is what `guarded_open_append` binds its descriptor to.
TRUNCATED_FILE_ID=""
safe_truncate() {
  ensure_output_file "$1" || return 1
  local guard
  if ! guard="$(fs_guard truncate "$ROOT" "$1" 2>&1)"; then
    path_fatal "$1 could not be truncated (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  TRUNCATED_FILE_ID="$guard"
  return 0
}

# guarded_write <absolute file below $ROOT>  - stdin becomes the whole file.
#
# The counterpart to `safe_truncate` for a file this framework writes in one
# go, and the reason it exists is that emptying a file safely is not the same
# as writing it safely. `safe_truncate` proves, on a descriptor, what it is
# about to empty - but a caller that then writes with `>"$file"` opens the
# NAME a second time, and everything the first check established is undone by
# whatever happened to the name in between (CWE-367). The bytes here never go
# near the existing name: `fs_guard write` creates a new inode O_EXCL inside
# the parent's descriptor, writes and flushes it, and renames it into place.
# A link, a hard link, a FIFO or a foreign-owned file at that name is
# replaced, not written through, and a reader of the name sees the whole old
# file or the whole new one.
guarded_write() {
  local file="$1"
  local dir="${file%/*}"

  # Only the directory is validated here. What is AT the name does not need to
  # be - and must not be refused - because nothing is written through it: the
  # bytes go into a new inode and the name is replaced by a rename. Refusing a
  # link planted at an output path would hand any process that can write into
  # the git-ignored tree a way to stop this run from recording anything, while
  # replacing it is both safe and correct.
  if [[ "$dir" == "$file" || -z "$dir" ]]; then
    path_fatal "$file has no directory part"
    return 1
  fi
  ensure_output_dir "$dir" || return 1
  local guard
  if ! guard="$(fs_guard write "$ROOT" "$file" 2>&1)"; then
    path_fatal "$file could not be written (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  return 0
}

# guarded_append <absolute file below $ROOT> - stdin is added to the end.
#
# Used where what is already in the file has to stay. `fs_guard append` opens
# it O_APPEND|O_NOFOLLOW and re-reads every property from the DESCRIPTOR it is
# about to write through, so the decision and the write are the same object.
guarded_append() {
  local file="$1"
  ensure_output_file "$file" || return 1
  local guard
  if ! guard="$(fs_guard append "$ROOT" "$file" 2>&1)"; then
    path_fatal "$file could not be appended to (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  return 0
}

# guarded_open_append <absolute file below $ROOT> <name of a variable>
#
# Creates the file, opens ONE append descriptor on it for the life of the run
# or the row, proves that descriptor is that file, and publishes the
# descriptor number in the named variable. Writing through it - `>&"$FD"`, or
# the `/dev/fd` path `guarded_fd_path` derives from it - reaches the inode
# that was validated and nothing else: there is no second lookup of the name,
# so renaming it, unlinking it or replacing it with a link afterwards cannot
# redirect a single byte (CWE-59, CWE-367).
#
# One pathname open remains, the `exec` below, and `fs_guard verify-fd` is
# what closes it: the descriptor is compared against the dev:ino
# `safe_truncate` had just validated, so a name replaced in that window is
# detected rather than written to.
guarded_open_append() {
  local file="$1"
  local variable="$2"
  local descriptor=""
  local identity

  safe_truncate "$file" || return 1
  identity="$TRUNCATED_FILE_ID"
  if ! exec {descriptor}>>"$file"; then
    path_fatal "$file could not be opened for appending"
    return 1
  fi
  local guard
  if ! guard="$(fs_guard verify-fd "$ROOT" "$file" "$descriptor" "$identity" 2>&1)"; then
    path_fatal "the descriptor opened for $file is not that file (${guard:-no reason given})"
    exec {descriptor}>&-
    return 1
  fi
  declare -g "$variable=$descriptor"
  return 0
}

# guarded_fd_path <descriptor> - the path form of a descriptor this run holds.
#
# `/proc/self/fd/N` resolves to the INODE the descriptor refers to, not by
# walking the evidence directory again, so `>>"$(guarded_fd_path "$FD")"`
# writes to the validated file even if its name has since been replaced by a
# link to somewhere else. Children inherit the descriptor, so a command, a
# block, a pipeline or a subshell redirected to this path writes to the same
# inode in the same append sequence.
guarded_fd_path() {
  # A descriptor this run does not hold must NOT produce "/proc/self/fd/",
  # which is the process's own descriptor DIRECTORY: `[[ -s ]]` reports a
  # directory as non-empty, so a reader guarded that way went on to `cat` it
  # and printed "Is a directory" instead of treating the evidence as absent.
  # A name nothing can open is the honest answer.
  if [[ ! "$1" =~ ^[0-9]+$ ]]; then
    printf '/proc/self/fd/none\n'
    return 0
  fi
  printf '/proc/self/fd/%s\n' "$1"
}

# guarded_close <name of the variable holding a descriptor> - closes it and
# clears the variable, so nothing can write through a descriptor whose row has
# ended.
guarded_close() {
  local variable="$1"
  local descriptor="${!variable:-}"
  if [[ -n "$descriptor" ]]; then
    exec {descriptor}>&-
    declare -g "$variable="
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
#
# Two kinds of directory, with two modes, because they have two audiences:
#
#   * target/, target/parity-report and target/test-reports are SHARED with
#     sbt and with the forked test JVMs that write the JUnit XML and the
#     parity reports, so they keep the conventional 0755. What this script
#     does own about them is that they are not group- or world-WRITABLE: a
#     writable evidence directory is where the links `fs_guard` refuses get
#     planted in the first place, so those bits are cleared when we own the
#     directory rather than treated as a reason to refuse an otherwise valid
#     tree.
#   * target/audit and everything below it is this run's own evidence - sbt
#     never writes there - so it is private, 0700. That is also the cheapest
#     answer to the runner metadata those logs carry: what is not readable is
#     not leaked to another account on the build host.
#-----------------------------------------------------------------------------

# The dev:ino of the two directories every later write depends on, as
# validated at startup. Recorded so that `verify_output_tree_identity` can
# prove, at any later point, that the tree being written into is still the one
# that was validated - a renamed or replaced parent is otherwise invisible to
# a run that only ever re-resolves the name.
TARGET_DIR_ID=""
AUDIT_DIR_ID=""

init_output_tree() {
  local dir
  # target/ stays 0755: sbt, coursier and the forked test JVMs all create
  # their own subdirectories in it, and tightening the directory they share
  # would be a change to the build rather than to this script's evidence.
  ensure_output_dir "$TARGET_DIR" 0755 || return 1
  TARGET_DIR_ID="$ENSURED_DIR_ID"
  # Everything this run's evidence lives in is private. The only other writer
  # of target/test-reports and target/parity-report is the forked test JVM,
  # which this script starts and which runs as the same uid, so 0700 costs the
  # build nothing and keeps the evidence - JUnit XML that carries the machine's
  # properties, parity reports, logs - out of every other account's reach
  # while it is being written (CWE-200).
  for dir in "$PARITY_DIR" "$TEST_REPORT_DIR" "$AUDIT_DIR" "$LOG_DIR" \
    "$SNAPSHOT_DIR" "$SNAPSHOT_DIR/test-reports" "$SNAPSHOT_DIR/parity-report"; do
    ensure_output_dir "$dir" 0700 || return 1
    if [[ "$dir" == "$AUDIT_DIR" ]]; then
      AUDIT_DIR_ID="$ENSURED_DIR_ID"
    fi
  done
  # sbt owns other subdirectories of target/, so only the three this script
  # writes evidence into are swept.
  for dir in "$AUDIT_DIR" "$PARITY_DIR" "$TEST_REPORT_DIR"; do
    assert_no_symlinks_below "$dir" || return 1
  done
  ensure_output_file "$REPORT_FILE" || return 1
  # Whatever an earlier run left in the evidence trees is examined and then
  # cleared, BEFORE this run writes anything of its own into them, so that
  # every artifact staged for publication later is this run's by construction
  # rather than by a guess about its timestamp. This is also the last moment
  # at which clearing them is possible: the two descriptors opened below, and
  # every row after them, write into these directories.
  prune_inherited_evidence || return 1
  for dir in "$LOG_DIR" "$SNAPSHOT_DIR" "$SNAPSHOT_DIR/test-reports" \
    "$SNAPSHOT_DIR/parity-report"; do
    ensure_output_dir "$dir" 0700 || return 1
  done
  # The appendix and the record of unchecked failures are opened once, on
  # descriptors, and everything written to them afterwards goes through those
  # descriptors. Everything above this line reports on stderr alone, so no
  # failure during validation can be written through a path the validation has
  # not yet cleared.
  guarded_open_append "$APPENDIX_FILE" APPENDIX_FD || return 1
  guarded_open_append "$FRAMEWORK_ERROR_FILE" FRAMEWORK_ERROR_FD || return 1
  FRAMEWORK_ERROR_READY="yes"
  # What the lock had to say, now that there is a validated path to say it on.
  # It is kept in the audit tree CI publishes rather than in the ledger above,
  # for the reason given at OUTPUT_LOCK_NOTE.
  if [[ -n "$OUTPUT_LOCK_NOTE" ]]; then
    local lock_record="$AUDIT_DIR/output-lock.txt"
    if ! printf '%s\n' "$OUTPUT_LOCK_NOTE" | guarded_write "$lock_record"; then
      printf 'WARNING: the output-lock note could not be recorded in %s: %s\n' \
        "${lock_record#"$ROOT"/}" "$OUTPUT_LOCK_NOTE" >&2
    fi
  fi
  # The tree is now exactly what was validated, and this is the baseline every
  # later check compares against: if it does not hold here, nothing recorded
  # afterwards can be attributed to this tree at all.
  verify_output_tree_identity || return 1
  return 0
}

# verify_output_tree_identity - re-opens target/ and target/audit no-follow and
# confirms they are still the same directories `init_output_tree` validated.
#
# A pathname check can only ever say what a name means now; this says whether
# it still means what it meant when the tree was created. It is called at the
# end of `init_output_tree` and is meant to be called again by anything that
# is about to publish or hand over the evidence, where a parent renamed
# mid-run would otherwise silently redirect the last step of the run.
# Returns 1 and reports through `path_fatal`; it never exits.
verify_output_tree_identity() {
  if [[ -z "$TARGET_DIR_ID" || -z "$AUDIT_DIR_ID" ]]; then
    path_fatal "the output tree has not been validated yet, so its identity cannot be re-checked"
    return 1
  fi
  local guard
  if ! guard="$(fs_guard verify-dir "$ROOT" "$TARGET_DIR" "$TARGET_DIR_ID" 2>&1)"; then
    path_fatal "$TARGET_DIR (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  if ! guard="$(fs_guard verify-dir "$ROOT" "$AUDIT_DIR" "$AUDIT_DIR_ID" 2>&1)"; then
    path_fatal "$AUDIT_DIR (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  return 0
}

#-----------------------------------------------------------------------------
# The output lock.
#
# Two acceptance runs in one checkout write the same evidence files, the same
# sbt logs and the same report, and neither knows the other exists: each
# truncates files the other is reading, `sbt clean` in one empties the
# directories the other has just snapshotted, and the report that survives is
# a mixture of two runs that describes neither. An acceptance run is not
# something to interleave, so a second one in the same checkout is refused
# rather than merged.
#
# The lock is a DIRECTORY, created with `mkdir`, because that is one atomic
# operation that fails if anything is already at the name - an existing
# directory, a file or a symbolic link - and it therefore needs no new tool
# on a host where `flock` may not be installed. The pid inside it is what
# tells a lock still held from one a killed run left behind.
#-----------------------------------------------------------------------------

# Set to "yes" only by the run that actually created the lock directory, so
# that a run which refused to start can never remove the lock of the run that
# is holding it.
OUTPUT_LOCK_HELD="no"

# What this run had to say about taking the lock, kept until there is a
# validated path to write it to. An abandoned lock means the previous run in
# this checkout did not finish, which is worth recording in the published
# evidence; it is NOT recorded in the framework-error ledger, because every
# entry there is blocking and this is not: the run that reclaims a lock goes
# on to do a full `clean` build and rewrite every evidence file, so the
# unfinished run's leftovers are replaced rather than read.
OUTPUT_LOCK_NOTE=""

# acquire_output_lock - takes the lock, or explains why this run must not run.
#
# Returns 0 with the lock held, or 1 with the reason on stderr (`init_run`
# turns that into exit 2). A lock whose recorded pid is still alive is a hard
# stop; one whose pid is gone is reclaimed, because a run killed by a CI
# timeout leaves its lock behind and the next run must not need a human to
# delete a directory. `kill -0` only asks whether the process exists - it
# sends no signal - and a pid we may not signal answers EPERM, which is
# "alive" and is the answer that stops this run. A pid that cannot be read at
# all is treated as alive too: the fail-closed side of that choice is a run
# that stops and says so.
acquire_output_lock() {
  # target/ has to exist to hold the lock, and it is validated here rather
  # than trusted, because this is the first thing that writes into it.
  ensure_output_dir "$TARGET_DIR" 0755 || return 1
  TARGET_DIR_ID="$ENSURED_DIR_ID"

  local owner_file="$OUTPUT_LOCK_DIR/owner"
  local attempt held_pid key value taken
  for attempt in 1 2; do
    # `fs_guard lock-dir`, not `mkdir -p` or `command mkdir`: a plain mkdir on
    # this pathname would create the lock THROUGH a symbolic link left in the
    # git-ignored target/, which would put the exclusion every other run
    # depends on outside the checkout (CWE-59). The mkdir inside `fs_guard`
    # happens relative to a descriptor for target/ opened no-follow, and its
    # success IS the atomic acquisition. An existing lock is the expected
    # answer here and not a framework failure to record.
    taken="$(fs_guard lock-dir "$ROOT" "$OUTPUT_LOCK_DIR" 2>/dev/null || printf 'refused')"
    if [[ "$taken" == refused ]]; then
      printf 'FATAL: the output lock %s could not be created; the output tree is\n' \
        "${OUTPUT_LOCK_DIR#"$ROOT"/}" >&2
      printf 'not in a state this run can take a lock in.\n' >&2
      return 1
    fi
    if [[ "$taken" != held ]]; then
      OUTPUT_LOCK_HELD="yes"
      if ! {
        printf 'pid\t%s\n' "$$"
        printf 'acquired\t%s\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
        printf 'script\t%s\n' "$SCRIPT_NAME"
      } | guarded_write "$owner_file"; then
        printf 'FATAL: the output lock %s could not record its owner, so a later\n' \
          "${OUTPUT_LOCK_DIR#"$ROOT"/}" >&2
        printf 'run could not tell a held lock from an abandoned one.\n' >&2
        return 1
      fi
      return 0
    fi

    if [[ "$attempt" -ne 1 ]]; then
      break
    fi

    held_pid=""
    # `! -L` as well as `-f`: the pid read here decides whether another run's
    # lock may be taken away, so a symbolic link planted at this name is not
    # followed. Without a pid this run refuses to reclaim, which is the safe
    # direction - it waits instead of interleaving.
    if [[ -f "$owner_file" && -r "$owner_file" && ! -L "$owner_file" ]]; then
      while IFS=$'\t' read -r key value || [[ -n "$key" ]]; do
        if [[ "$key" == "pid" ]]; then
          held_pid="$value"
        fi
      done <"$owner_file"
    fi

    if [[ -n "$held_pid" && "$held_pid" =~ ^[0-9]+$ ]] && ! kill -0 "$held_pid" 2>/dev/null; then
      printf 'NOTICE: the output lock %s was left behind by process %s, which is no\n' \
        "${OUTPUT_LOCK_DIR#"$ROOT"/}" "$held_pid" >&2
      printf 'longer running. Reclaiming it and continuing.\n' >&2
      OUTPUT_LOCK_NOTE="$(printf 'reclaimed the output lock %s from process %s, which is no longer running, so the previous run in this checkout did not finish' \
        "${OUTPUT_LOCK_DIR#"$ROOT"/}" "$held_pid")"
      local guard
      if ! guard="$(fs_guard rmtree "$ROOT" "$OUTPUT_LOCK_DIR" 2>&1)"; then
        printf 'FATAL: the abandoned output lock %s could not be removed (%s).\n' \
          "${OUTPUT_LOCK_DIR#"$ROOT"/}" "${guard:-no reason given}" >&2
        return 1
      fi
      continue
    fi

    printf 'FATAL: another acceptance run holds the output lock %s\n' \
      "${OUTPUT_LOCK_DIR#"$ROOT"/}" >&2
    printf '(recorded pid: %s). Two runs in one checkout overwrite each other'"'"'s\n' \
      "${held_pid:-not recorded}" >&2
    printf 'evidence, logs and report, so this run stops instead of interleaving\n' >&2
    printf 'with it. Wait for that run to finish; if you are certain no run is\n' >&2
    printf 'in progress, remove that directory and re-run.\n' >&2
    return 1
  done

  printf 'FATAL: the output lock %s could not be taken.\n' \
    "${OUTPUT_LOCK_DIR#"$ROOT"/}" >&2
  return 1
}

# release_output_lock - gives the lock up, but only if this run took it.
#
# Called from the EXIT path AFTER the report has been written, so releasing it
# can neither replace nor race the one artifact the run exists to produce. A
# run that dies before this point leaves the directory behind; the next run
# sees its pid is gone and reclaims it, which is why this needs no signal
# handler of its own.
release_output_lock() {
  if [[ "$OUTPUT_LOCK_HELD" != "yes" ]]; then
    return 0
  fi
  OUTPUT_LOCK_HELD="no"
  local guard
  if ! guard="$(fs_guard rmtree "$ROOT" "$OUTPUT_LOCK_DIR" 2>&1)"; then
    printf 'WARNING: this run could not release its output lock %s (%s).\n' \
      "${OUTPUT_LOCK_DIR#"$ROOT"/}" "${guard:-no reason given}" >&2
    printf 'The next run will reclaim it once this process has exited.\n' >&2
    return 1
  fi
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
  assert_root_is_safe "$ROOT" || return 1
  assert_root_is_safe "$ROOT_REAL" || return 1
  return 0
}

# assert_root_is_safe <path> - refuses a checkout path this script cannot
# safely put into the things it generates.
#
# ROOT comes from `git rev-parse --show-toplevel`, which is to say from
# whatever directory the checkout happens to sit in, and it ends up in three
# kinds of output: shell words (safely quoted), SCALA SOURCE handed to sbt's
# `set` command, and Markdown table cells. The second and third are the
# injection surfaces - a `"` in a directory name closes the Scala string
# literal that surrounds it and the rest of the path becomes an expression
# sbt evaluates (CWE-94), and a newline or a pipe forges rows in the report
# (CWE-117). Both of those are also handled where they occur, by
# `scala_string_literal` and `markdown_cell`; this check is the strongest of
# the three measures because it is the only one that removes the dangerous
# input from the run entirely instead of encoding it correctly at each of the
# dozens of places it is used, and because it fails loudly at the first
# instruction of the run rather than in whatever generated artifact was
# reached first.
#
# The accepted set is deliberately conservative - letters, digits, the space,
# and the handful of punctuation characters a real checkout path uses -
# because this is a build-machine path, not user data: a path outside that set
# is far more likely to be an attempt at this than a directory somebody meant
# to create.
#
# The SPACE is accepted, and that is a decision rather than an oversight. It
# is not an injection character anywhere this path is used: it needs no
# escaping inside a Scala string literal, it is ordinary text in a Markdown
# cell, and every shell expansion of a path in this script is quoted (the one
# that was not - `javac -d dir $(find ...)` in the construction-closure row -
# is a `-print0` array read now, which is what makes accepting the space a
# supported case instead of a hope). Refusing it bought nothing and cost the
# whole run: a checkout under a directory whose name contains a space could
# not be gate-verified at all, and "the delivery is unacceptable" and "this
# script will not look at it" are not the same answer.
assert_root_is_safe() {
  local path="$1"

  if [[ "$path" != /* ]]; then
    printf 'FATAL: the repository root %s is not an absolute path.\n' "$path" >&2
    return 1
  fi
  # `[[:cntrl:]]` covers every control character including the newline and the
  # tab, which are the two that forge structure in a generated document.
  if [[ "$path" == *[[:cntrl:]]* ]]; then
    printf 'FATAL: the repository root contains a control character.\n' >&2
    printf 'This script generates Scala source and a Markdown report that\n' >&2
    printf 'contain the checkout path, so such a path is refused rather than\n' >&2
    printf 'encoded. Move the checkout to a plain path and re-run.\n' >&2
    return 1
  fi
  if [[ "$path" == *[^A-Za-z0-9/._+@%,=:~\ -]* ]]; then
    printf 'FATAL: the repository root %s contains a character this script\n' "$path" >&2
    printf 'will not put into the Scala source it hands to sbt or into the\n' >&2
    printf 'Markdown report it writes. Quotation marks, backslashes,\n' >&2
    printf 'backticks and dollar signs are refused here rather than escaped\n' >&2
    printf 'in every one of the places the path is used. Move the checkout to\n' >&2
    printf 'a path made of letters, digits, spaces and . _ + @ %% , = : ~ -\n' >&2
    printf 'and re-run.\n' >&2
    return 1
  fi
  return 0
}

# scala_string_literal <text> - prints <text> as a Scala string literal,
# quotation marks included, or refuses.
#
# Used where this script has to put a path into Scala SOURCE - the one place
# where a value crosses from shell into a language sbt compiles and evaluates.
# Shell quoting does nothing there: `"..."` around the whole `set` command
# keeps the shell from splitting it, and leaves the Scala parser reading every
# character of the interpolated path as syntax. So the backslash is escaped
# first (escaping it after the quotation mark would double-escape the ones
# this function itself inserts), then the quotation mark, and a control
# character is REFUSED rather than encoded: Scala's `\n` and friends would be
# correct, but a path containing one has no business reaching a generated
# source file at all, and refusing keeps this function's output to the printable
# subset a human reviewing the sbt command line can read.
#
# Returns 1 and prints nothing when it refuses, so a caller that ignores the
# status produces an empty argument rather than an unescaped one.
scala_string_literal() {
  local text="$1"

  if [[ "$text" == *[[:cntrl:]]* ]]; then
    return 1
  fi
  text="${text//\\/\\\\}"
  text="${text//\"/\\\"}"
  printf '"%s"' "$text"
  return 0
}

# markdown_cell <text> - prints <text> so that it cannot forge the structure
# of the report it is written into.
#
# The report is a Markdown table, and a value interpolated into a cell can
# leave that cell: `|` starts the next column, a newline starts the next ROW
# (and a carriage return moves a terminal's cursor over what was already
# printed), a backtick opens code formatting that swallows everything after
# it, and a stray control character can make the whole document unreadable in
# a viewer (CWE-117 log/output injection). Every one of those arrives from
# somewhere this script does not control: a row's detail quotes file paths,
# git output, compiler messages and tool version strings.
#
# It is written with parameter expansion and `printf` alone - no `sed`, no
# `tr`, no `python3` - because the report is the one artifact that must still
# be producible when something about the environment is broken, and reaching
# for an external tool to escape a cell would make the report depend on the
# very thing that failed.
#
# The order matters: the backslash is escaped first, so the escapes added
# afterwards are not themselves doubled, and the newline markers inserted last
# are therefore distinguishable from a literal backslash-n in the input (which
# has become `\\n` by then). The length is bounded before any escaping, so the
# bound applies to the text a reader sees and a truncated cell can never end
# in a half-written escape.
MARKDOWN_CELL_LIMIT=500
markdown_cell() {
  local text="$1"

  # A tab is a control character, but blanking it would run two words
  # together, so it becomes the space it was standing in for.
  text="${text//$'\t'/ }"
  text="${text//$'\r\n'/$'\n'}"
  # Every remaining control character EXCEPT the newline, which the last
  # substitution below turns into a visible marker instead. A lone carriage
  # return is among the ones stripped here: it is not a line break, it is a
  # cursor movement that hides what was already printed.
  text="${text//[$'\001'-$'\011'$'\013'-$'\037'$'\177']/}"
  if [[ "${#text}" -gt "$MARKDOWN_CELL_LIMIT" ]]; then
    text="${text:0:$MARKDOWN_CELL_LIMIT} [truncated to $MARKDOWN_CELL_LIMIT characters]"
  fi
  text="${text//\\/\\\\}"
  text="${text//|/\\|}"
  text="${text//\`/\\\`}"
  text="${text//$'\n'/\\n}"
  printf '%s' "$text"
}

#-----------------------------------------------------------------------------
# Run identity.
#
# An acceptance artifact that does not say what it measured cannot be held
# against anything. A gate report, a parity report and a JUnit XML file are
# all indistinguishable from the ones a run produced days and several commits
# earlier - the XML even carries its own timestamp, which is the timestamp of
# the run that wrote it and says nothing about the code it exercised
# (CWE-345 insufficient verification of data authenticity). So every run
# stamps WHICH commit, WHICH run and WHEN into `target/audit/run-identity.txt`,
# into the report's header and into the published tree, and refuses to publish
# any artifact written before it started (see `finalize_publication`): those two
# together are what make the evidence answer for the tree in front of the
# reader rather than for whatever was last built here.
#
# The commit fields come from git and are therefore untrusted text - a commit
# subject is whatever somebody wrote - so every one of them is passed through
# `markdown_cell` before it is recorded.
#-----------------------------------------------------------------------------

RUN_ID=""
RUN_STARTED_UTC=""
# Seconds since the epoch, taken as the FIRST thing `init_run` does, so that
# every file this run writes - including the ones written while the output
# tree is still being validated - has a modification time at or after it.
RUN_STARTED_EPOCH=""
HEAD_COMMIT=""
HEAD_SUBJECT=""
HEAD_BRANCH=""
HEAD_STATE=""

# init_run_identity - resolves the identity of this run and records it.
#
# Called by `init_run` once the audit tree exists. Returns 1 only if the
# record itself cannot be written: git answering "unknown" is a fact about
# the checkout that is reported rather than a reason to stop, because a
# detached or grafted checkout still produces evidence - it just cannot claim
# a commit.
init_run_identity() {
  HEAD_COMMIT="$(git rev-parse HEAD 2>/dev/null || printf 'unknown')"
  HEAD_SUBJECT="$(markdown_cell "$(git log -1 --pretty=%s 2>/dev/null || printf 'unknown')")"
  HEAD_BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || printf 'unknown')"

  # Whether the tree the gates measured is the commit they name. A dirty tree
  # is not a failure - a developer runs this before committing - but a report
  # that did not say so would be attributed to a commit that never contained
  # what was measured.
  local dirty dirty_count
  if dirty="$(git status --porcelain 2>/dev/null)"; then
    dirty_count="$(printf '%s' "$dirty" | command awk 'NF { n++ } END { print n + 0 }')"
    if [[ "$dirty_count" -eq 0 ]]; then
      HEAD_STATE="clean"
    else
      HEAD_STATE="$dirty_count uncommitted path(s), so the measured tree is NOT exactly this commit"
    fi
  else
    HEAD_STATE="unknown"
  fi

  if ! {
    printf '# the identity of this acceptance run, written before the first row\n'
    printf 'run-id\t%s\n' "$RUN_ID"
    printf 'started-utc\t%s\n' "$RUN_STARTED_UTC"
    printf 'started-epoch\t%s\n' "$RUN_STARTED_EPOCH"
    printf 'script\t%s\n' "$SCRIPT_NAME"
    printf 'commit\t%s\n' "$HEAD_COMMIT"
    printf 'commit-subject\t%s\n' "$HEAD_SUBJECT"
    printf 'branch\t%s\n' "$HEAD_BRANCH"
    printf 'working-tree\t%s\n' "$HEAD_STATE"
  } | guarded_write "$RUN_IDENTITY_FILE"; then
    printf 'FATAL: the run identity could not be recorded in %s.\n' \
      "${RUN_IDENTITY_FILE#"$ROOT"/}" >&2
    return 1
  fi
  printf 'run %s at commit %s (%s), working tree: %s\n' \
    "$RUN_ID" "$HEAD_COMMIT" "$HEAD_BRANCH" "$HEAD_STATE"
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

  # The first measurements of the run, taken before anything is created: the
  # instant it started, and the name everything it produces is filed under.
  # Both are needed by `init_output_tree` - the sweep of inherited evidence
  # files what it quarantines under this run's id - so they are taken here and
  # the identity RECORD, which needs the audit tree, is written later.
  RUN_STARTED_EPOCH="$(date -u '+%s')"
  RUN_STARTED_UTC="$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
  RUN_ID="$(printf '%s-%s' "$(date -u '+%Y%m%dT%H%M%SZ')" "$$")"

  parse_arguments "$@"
  resolve_locations
  require_repository_root || exit 2
  # Before anything is created or emptied: this run becomes the only one
  # writing into this checkout's target/. It has to be taken here rather than
  # after the tree is built, because building the tree is itself a set of
  # truncations that a second run would interleave with. The EXIT trap is not
  # installed yet, so a failure between here and `install_traps` leaves the
  # lock directory behind - which is exactly the abandoned lock the next run
  # reclaims from the recorded pid, so it heals without a human.
  acquire_output_lock || exit 2
  # The output tree is validated and created BEFORE the checked-command
  # wrappers exist, so nothing can be recorded through a path that has not
  # been proved safe yet; `init_output_tree` opens the record itself, as its
  # last step.
  init_output_tree || exit 2
  # Which commit and which run every artifact below belongs to. It needs the
  # audit tree, so it comes after it, and it comes before the traps so that a
  # report written by an early exit already carries the identity.
  init_run_identity || exit 2
  install_checked_commands
  # `install_traps` is defined with the traps themselves, next to the report.
  install_traps
}

#-----------------------------------------------------------------------------
# Gate bookkeeping. Six ordered, parallel arrays: the row label, its verdict,
# a one-line detail, the measurement command(s) it ran, the relative path of
# its evidence, and its kind - `automated` for a measured row, `reported` for
# the one row that is stated rather than measured, `preflight` for the tool
# check that precedes them all, and `blocking` for a check that is neither a
# gate nor a tool check but stops the run anyway.
# The arrays are the single source of truth for every count in the report,
# which is computed by tallying their entries: no count is ever derived by
# subtracting one running total from another, because a row recorded under one
# tally and not the other then yields an impossible figure.
#-----------------------------------------------------------------------------

GATE_LABEL=()
GATE_STATUS=()
GATE_DETAIL=()
# What each row RAN. The validation table of the specification defines every
# row as a measurement command, so a report that states the verdict and the
# detail but not the command leaves the reader unable to tell what was
# measured without reading the script - and unable to re-run it by hand. The
# strings here are the same ones the row's evidence file prints on its
# `# command:` lines, because `command_line` writes the line and records the
# cell from one argument; see `record_command`.
GATE_COMMAND=()
GATE_EVIDENCE=()
GATE_KIND=()
GATE_FAILED=0
REPORT_WRITTEN="no"
# Set when the first assembly of the report carried a credential signature
# and the report had to be reassembled without its appendices. Read by the
# assembly itself, which says so where the appendices would have been.
REPORT_SANITIZED="no"
# The number of automated rows AAP section 0.10.1 defines, so that a report
# written by the EXIT trap after an interruption can say how much of the run
# it covers instead of presenting a partial result as an acceptance result.
GATE_EXPECTED_AUTOMATED=20
RUN_COMPLETED="no"

GATE_DETAIL_OUT=""
GATE_EVIDENCE_OUT=""
GATE_COMMAND_OUT=""

# The Command column of the two kinds of row that measure nothing. They are
# named constants rather than inline defaults because the text they hold
# carries an apostrophe, and `"${5:-a word's default}"` is quote-processed by
# bash: the apostrophe inside a parameter-expansion default opens a quoted
# string and everything after it is swallowed until the next one.
GATE_COMMAND_OUTSIDE_TABLE="not one of the measurements of the validation table: a check outside it that stops the run, described in the detail"
GATE_COMMAND_REPORTED="not measured by this script: an out-of-band pull-request review by a CODEOWNERS owner"

# Tallies of the arrays above, recomputed by `gate_counts`. One implementation
# feeds both the report and the closing summary, so the two cannot disagree.
GATE_COUNT_AUTOMATED=0
GATE_COUNT_PASSED=0
GATE_COUNT_FAILED=0
GATE_COUNT_REPORTED=0
GATE_COUNT_PREFLIGHT_FAILED=0
# Counted separately from the preflight row, because they are different facts
# and the report states them differently: "the toolchain was incomplete" is
# not true of a run stopped by a security check, and a report that says so
# sends whoever reads it to look for a missing tool.
GATE_COUNT_BLOCKING_FAILED=0
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
  GATE_COUNT_BLOCKING_FAILED=0
  GATE_ARRAY_PROBLEM=""

  local rows="${#GATE_LABEL[@]}"
  if [[ "${#GATE_STATUS[@]}" -ne "$rows" || "${#GATE_DETAIL[@]}" -ne "$rows" ||
    "${#GATE_COMMAND[@]}" -ne "$rows" || "${#GATE_EVIDENCE[@]}" -ne "$rows" ||
    "${#GATE_KIND[@]}" -ne "$rows" ]]; then
    GATE_ARRAY_PROBLEM="$(printf 'the gate arrays have diverged: %s labels, %s statuses, %s details, %s commands, %s evidence paths, %s kinds' \
      "$rows" "${#GATE_STATUS[@]}" "${#GATE_DETAIL[@]}" "${#GATE_COMMAND[@]}" \
      "${#GATE_EVIDENCE[@]}" "${#GATE_KIND[@]}")"
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
      blocking)
        # A check outside the validation table that stops the run: the
        # publication-artifact secret scan of what a previous run left behind
        # is one. Blocking, but not a measured row and not a missing tool.
        if [[ "${GATE_STATUS[$index]:-}" != "PASS" ]]; then
          GATE_COUNT_BLOCKING_FAILED=$((GATE_COUNT_BLOCKING_FAILED + 1))
        fi
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
# The descriptor `init_output_tree` opened on the ledger, and the one it opened
# on the appendix file. Both are written through and READ BACK through their
# descriptors: the ledger's line count decides whether the run failed outside
# a row, so a file substituted at that name could otherwise hide a failure by
# being empty, and `/proc/self/fd/N` reaches the inode rather than the name.
FRAMEWORK_ERROR_FD=""
APPENDIX_FD=""

framework_error() {
  if [[ "$FRAMEWORK_ERROR_READY" == "yes" && -n "$FRAMEWORK_ERROR_FD" ]]; then
    # Through the descriptor's own path rather than `>&"$FD"`: both write to
    # the inode the descriptor holds, and this form leaves the failure of the
    # write itself silenceable without two redirections competing for stderr.
    printf '%s\n' "$1" >>"$(guarded_fd_path "$FRAMEWORK_ERROR_FD")" 2>/dev/null || true
  fi
  printf 'FRAMEWORK ERROR: %s\n' "$1" >&2
}

# The number of failures recorded so far; 0 when nothing has been recorded.
framework_error_count() {
  if [[ "$FRAMEWORK_ERROR_READY" == "yes" && -n "$FRAMEWORK_ERROR_FD" ]]; then
    command awk 'END { print NR + 0 }' "$(guarded_fd_path "$FRAMEWORK_ERROR_FD")" \
      2>/dev/null || printf '0\n'
  else
    printf '0\n'
  fi
}

# The failures recorded after the first <n> lines, i.e. those belonging to the
# row that has just run.
framework_errors_since() {
  local skip="$1"
  if [[ "$FRAMEWORK_ERROR_READY" == "yes" && -n "$FRAMEWORK_ERROR_FD" ]]; then
    command awk -v skip="$skip" 'NR > skip { print }' \
      "$(guarded_fd_path "$FRAMEWORK_ERROR_FD")" 2>/dev/null || true
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

detail() {
  if [[ -z "$GATE_DETAIL_OUT" ]]; then
    GATE_DETAIL_OUT="$1"
  else
    GATE_DETAIL_OUT="$GATE_DETAIL_OUT; $1"
  fi
}

# record_command <command> - adds one measurement command to the current row,
# for the Command column of the report.
#
# Repeats are dropped: a row that runs the same grep over two trees, or that
# names a command once in its own evidence and once through a shared helper,
# should read as having run it once. Several DIFFERENT commands accumulate in
# the order they ran, separated like `detail`'s clauses, because most rows
# measure their pass condition with more than one.
record_command() {
  local command_text="$1"

  [[ -n "$command_text" ]] || return 0
  if [[ -z "$GATE_COMMAND_OUT" ]]; then
    GATE_COMMAND_OUT="$command_text"
    return 0
  fi
  case "; $GATE_COMMAND_OUT; " in
    *"; $command_text; "*) return 0 ;;
  esac
  GATE_COMMAND_OUT="$GATE_COMMAND_OUT; $command_text"
}

# command_line <command> - prints the evidence file's `# command:` line for
# <command> and records the same string for the report's Command column.
#
# One argument feeding both is the whole point: the command a reader finds in
# the evidence and the command the report attributes to the row cannot drift
# apart, because there is only one string. It prints to STDOUT, so it is
# called from inside the block a row redirects into its evidence file - a
# brace group, which is not a subshell, so the record survives it. It must
# NOT be called inside a pipeline or a command substitution, where the
# assignment would be discarded with the subshell; `record_command` on its own
# is the form for those places.
command_line() {
  record_command "$1"
  printf '# command: %s\n' "$1"
}

evidence() {
  GATE_EVIDENCE_OUT="${1#"$ROOT"/}"
}

# Starts a fresh evidence file for the current row and publishes it in EV.
# Not a command substitution: that would run in a subshell and the `evidence`
# assignment would be lost.
#
# EV is what every row redirects into, and it is a DESCRIPTOR path rather than
# the evidence file's name. The file is created and validated once, one append
# descriptor is opened on it and proved to be that file, and EV is the
# `/proc/self/fd` form of that descriptor - so each of the row's `>>"$EV"`
# writes reaches the inode this framework validated, and replacing the
# evidence file's NAME with a link to somewhere else afterwards redirects
# nothing (CWE-59, CWE-367). Rows are unchanged by this: the redirection they
# already write is what becomes descriptor-bound.
#
# EV_PATH is the same file by name, for the things a name is for - the report
# column, the publication manifest, a reader looking for it on disk.
EV=""
EV_PATH=""
EV_FD=""
new_evidence() {
  guarded_close EV_FD
  EV_PATH="$AUDIT_DIR/$1"
  evidence "$EV_PATH"
  # Checked and link-safe: a row whose evidence file cannot be started has
  # nowhere to record what it measured, and the record makes that the row's
  # verdict even though every call site ignores this status. EV then names the
  # null device rather than the refused path: losing a row's evidence and
  # failing the run is the lesser harm, where writing it through a name this
  # framework has just refused is the harm the refusal exists to prevent.
  if ! guarded_open_append "$EV_PATH" EV_FD; then
    EV="/dev/null"
    framework_error "the evidence file $1 could not be started, so this row's evidence was discarded rather than written through an unsafe path"
    return 1
  fi
  EV="$(guarded_fd_path "$EV_FD")"
  return 0
}

# Appends one appendix section to the report's appendix file. Content arrives
# on stdin so that a caller can pipe a file, a command or a here-document. A
# failed append is recorded: an appendix silently missing from the report
# would be indistinguishable from a row that had nothing to report.
add_appendix() {
  local title="$1"
  if [[ -z "$APPENDIX_FD" ]]; then
    framework_error "the appendix \"$title\" could not be appended: the appendix file is not open"
    return 1
  fi
  # The fence is sized to the content instead of being a fixed three
  # backticks. Appendix content is evidence - sbt output, a javap dump, a scan
  # summary - and a fixed fence is ended by the first line of that evidence
  # which happens to contain three backticks: everything after it is then
  # document structure rather than quoted text, so a crafted line can add a
  # table row or a RESULT line to the published report (CWE-117 log injection
  # leading to CWE-345 insufficient verification). A fence one backtick longer
  # than the longest run in the content cannot be ended by the content, which
  # is what CommonMark's fenced-block rule guarantees. The section is written
  # through the appendix descriptor, as before.
  if ! python3 /dev/fd/3 "$title" 3<<'PY' >>"$(guarded_fd_path "$APPENDIX_FD")"
import re
import sys

title = sys.argv[1]
content = sys.stdin.buffer.read().decode("utf-8", "surrogateescape")
if content and not content.endswith("\n"):
    content += "\n"
longest = max((len(run) for run in re.findall("`+", content)), default=0)
fence = "`" * max(3, longest + 1)
sys.stdout.write("\n### %s\n\n%stext\n%s%s\n" % (title, fence, content, fence))
PY
  then
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
  GATE_COMMAND_OUT=""

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

  # A row that recorded no measurement command has not said WHAT it measured,
  # and the report would carry a verdict nobody can reproduce by hand. Every
  # row names its commands through `command_line` or `record_command`, so an
  # unrecorded one is a defect in the row - the same reasoning, and the same
  # verdict, as a row that declared no evidence file.
  if [[ -z "$GATE_COMMAND_OUT" ]]; then
    detail "this row recorded no measurement command"
    rc=1
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
  GATE_COMMAND+=("${GATE_COMMAND_OUT:-no command recorded}")
  GATE_EVIDENCE+=("${GATE_EVIDENCE_OUT:-none}")
  GATE_KIND+=("automated")

  printf '%s: %s -- %s\n' "$status" "$label" "${GATE_DETAIL_OUT:-no detail recorded}"
  return 0
}

# record_blocking_row <label> <detail> <evidence> [kind] [command]
#
# Records a FAILED check that is not one of the table's rows and stops the
# run: the preflight tool check, and the pre-run scan of the publication
# artifacts a previous run left behind. `kind` defaults to `blocking`; the
# preflight check passes `preflight`, because the report says something
# different about a missing toolchain. `command` is what the check ran, for
# the report's Command column; it defaults to a statement that the row is not
# one of the table's measurements, which is true of every one of these rows
# and is what the column should say rather than nothing.
#
# It exists because those two branches used to do this by hand, and one of
# them did it wrong: it appended four of the five parallel arrays and left
# GATE_KIND alone, so `gate_counts` would have reported the arrays as
# diverged - and before it could, the branch incremented a GATE_TOTAL that is
# assigned nowhere else in this file, which under `set -u` aborts the branch
# on that line. The result was that the security failure path exited without
# writing the report it was written to write. One helper, five appends, one
# failure counter: a branch that records a blocking result cannot now record
# half of one.
record_blocking_row() {
  GATE_LABEL+=("$1")
  GATE_STATUS+=("FAIL")
  GATE_DETAIL+=("${2:-no detail recorded}")
  GATE_COMMAND+=("${5:-$GATE_COMMAND_OUTSIDE_TABLE}")
  GATE_EVIDENCE+=("${3:-none}")
  GATE_KIND+=("${4:-blocking}")
  GATE_FAILED=$((GATE_FAILED + 1))
  printf '\n=== %s ===\nFAIL: %s -- %s\n' "$1" "$1" "${2:-no detail recorded}" >&2
}

# Records a row that is reported rather than measured. Its verdict text is
# fixed by the specification and is written verbatim into the report; its
# Command column says so rather than naming a command, because there is no
# command - that is what "reported rather than measured" means.
record_reported_row() {
  GATE_LABEL+=("$1")
  GATE_STATUS+=("REPORTED")
  GATE_DETAIL+=("$2")
  GATE_COMMAND+=("${4:-$GATE_COMMAND_REPORTED}")
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
    # Through `command_line`, so the grep this helper ran for the calling row
    # reaches that row's Command column and not only its evidence file. The
    # block is a brace group, so the record survives it.
    command_line "grep $*"
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
  # (git, sbt, java, javap, javac - the last compiling the two attacks of the
  # JVM-closure row), the parsers (python3, awk, sed, grep), the text
  # utilities the rows transform evidence with (find, sort, comm, tr, cut, wc,
  # diff, cmp, uniq, cat, head, tail, basename, date) and the file operations
  # the snapshots, logs and the report depend on (cp, rm, mkdir, mv, tee). A
  # tool absent from this list is a tool whose absence would first be noticed
  # halfway through a row, as a failure of that row rather than of the
  # environment.
  local required=(
    git sbt java javap javac python3
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
  # Started through the same guard every other evidence file goes through
  # (`new_evidence` does it for the rows): the audit tree was swept for links
  # when it was created, but this is the first write into it after that sweep,
  # and a redirected preflight record is the one piece of evidence that says
  # what this run's toolchain actually was.
  if ! safe_truncate "$evidence_file"; then
    printf 'FATAL: the preflight evidence file %s could not be started, so this\n' \
      "${evidence_file#"$ROOT"/}" >&2
    printf 'run cannot record which tools it found. See the reason above.\n' >&2
    record_blocking_row "Preflight - required tools" \
      "the preflight evidence file ${evidence_file#"$ROOT"/} could not be started" \
      none \
      preflight
    write_report
    exit 2
  fi
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
  } | guarded_write "$evidence_file"

  if [[ "${#missing[@]}" -gt 0 ]]; then
    printf 'FATAL: preflight failed - these required tools are not on PATH: %s\n' \
      "${missing[*]}" >&2
    printf 'A complete run needs JDK 21 and sbt 1.13.0, as installed by the\n' >&2
    printf 'CircleCI scala_build21 job. Install them and re-run; gates are never\n' >&2
    printf 'skipped because a tool is missing.\n' >&2
    # Recorded as a `preflight` row, not an automated one: no gate of the
    # specification's table ran, and counting it among them would make the
    # report claim a measurement that never happened. Through the one helper
    # that appends all five arrays, so this branch and the pre-run scan's
    # branch cannot record a result two different ways.
    record_blocking_row "Preflight - required tools" \
      "missing tools: ${missing[*]}" \
      "${evidence_file#"$ROOT"/}" \
      preflight
    # Through the transaction, so the report that is published is the one that
    # states this row; the EXIT trap would reach it anyway and this keeps the
    # ordering of this path explicit. Exit 2 stands for the preflight failure
    # regardless of the publication's own verdict, which it states itself.
    if ! finalize_publication; then
      printf 'The evidence of this aborted run was not approved for publication.\n' >&2
    fi
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
#
# Both ends are validated first, which `mkdir -p` plus a bare `cp -a` did not
# do at all. The destination goes through `ensure_output_dir`, so it is below
# the repository root, has no symlinked component and is not created through
# one. Both trees are then swept for symbolic links, because `cp` follows a
# link it finds at the DESTINATION and would write the copied bytes wherever
# it points, and a link in the SOURCE is content this run never produced.
# Neither tree is written by anything that creates links - sbt's reporters and
# this script's own snapshots - so a link in either is a planted one and the
# copy is refused rather than performed.
copy_directory_contents() {
  local source_dir="$1"
  local destination="$2"

  if [[ ! -d "$source_dir" ]]; then
    return 0
  fi
  if ! assert_no_symlinks_below "$source_dir"; then
    snapshot_failure "$source_dir contains a symbolic link, so it was not copied into $destination"
    return 1
  fi
  if ! ensure_output_dir "$destination"; then
    snapshot_failure "could not create $destination"
    return 1
  fi
  if ! assert_no_symlinks_below "$destination"; then
    snapshot_failure "$destination contains a symbolic link, so $source_dir was not copied into it"
    return 1
  fi
  # `fs_guard copy-tree`, not `cp -a`: `cp` resolves every name itself, so in
  # a git-ignored tree it follows whatever link or replaced parent it finds,
  # and the two sweeps above can only say what the trees looked like a moment
  # earlier (CWE-59, CWE-367). Every file is read from a descriptor proved to
  # be a regular, singly-linked file of this run's, and every copy is created
  # O_EXCL inside a descriptor for its destination directory.
  local copied
  if ! copied="$(fs_guard copy-tree "$ROOT" "$source_dir" "$destination" 2>&1)"; then
    snapshot_failure "could not copy $source_dir into $destination (${copied:-no reason given})"
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
#
# The emptying is `fs_guard rmtree`, not `rm -rf`. `rm -rf` walks by name: it
# is given a path in a git-ignored tree and told to recurse and delete, and
# what it deletes is decided by what the names mean as it goes. The
# descriptor-relative delete instead opens every directory no-follow from its
# parent, refuses to descend a symbolic link (it unlinks the link itself and
# stops there), refuses to cross onto another filesystem, and refuses any path
# that is not below target/ - target/ itself included. The destination is then
# recreated private, because every mirror destination is under target/audit.
mirror_directory() {
  local source_dir="$1"
  local destination="$2"
  local guard

  if ! guard="$(fs_guard rmtree "$ROOT" "$destination" 2>&1)"; then
    snapshot_failure "could not clear $destination (${guard:-fs_guard refused it without a reason, which is itself a defect})"
    return 1
  fi
  if ! ensure_output_dir "$destination" 0700; then
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
    command_line 'sbt -batch clean compile Test/compile test'
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

  # The XML's own account of the same run: the `tests` attribute of the
  # `testsuite` element of each snapshotted report, summed. The documents are
  # PARSED rather than grepped, so the count is the attribute of the suite
  # element itself - a suite that prints the text `tests="n"` from a test of
  # its own cannot inflate the total - and a report truncated mid-write is a
  # parse error here rather than a number that happens to be readable.
  local xml_tests xml_tests_rc=0
  xml_tests="$(python3 -c 'import pathlib, sys, xml.etree.ElementTree as ET
total = 0
for path in sorted(pathlib.Path(sys.argv[1]).glob("TEST-*.xml")):
    try:
        total += int(ET.parse(path).getroot().get("tests") or 0)
    except Exception as error:
        sys.stderr.write("unparseable JUnit report " + str(path) + ": " + str(error) + "\n")
        raise SystemExit(1)
print(total)' "$SNAPSHOT_DIR/test-reports" 2>>"$EV")" || xml_tests_rc=$?
  if [[ "$xml_tests_rc" -ne 0 ]]; then
    detail "the snapshotted JUnit XML could not be counted (the parser exited $xml_tests_rc; see the evidence file for the report it rejected)"
    failed=1
    xml_tests=0
  fi

  # Two independent accounts of one run, and the DIRECTION of a disagreement is
  # the diagnosis.
  #
  # Fewer tests in the XML than in the log means reports were lost between the
  # run and the artifact this gate and the test-scope row count from, which is
  # exactly the failure this row exists to refuse: an incomplete report set
  # whose suites all say `failures="0"` reads like a passing run. The build
  # itself now refuses such a run (`reportAuditingTestResultLogger` in
  # build.sbt), so reaching this check means that refusal was bypassed or
  # defeated, and the row fails rather than reporting a number it cannot trust.
  #
  # More tests in the XML than in the log is the opposite case and not a
  # failure: ScalaTest's framework summary is printed from the events that
  # reached its reporter, so a reporter that broke mid-run understates the log
  # while the XML - completed by the build from sbt's own test events - remains
  # whole. It is recorded, with the breakage count, so the difference is never
  # silent.
  local reporter_breakages promotions
  reporter_breakages="$(grep -c "Reporter completed abruptly" "$SBT_LOG" || true)"
  promotions="$(grep -c "were reported incompletely by ScalaTest" "$SBT_LOG" || true)"
  if [[ "$xml_tests_rc" -eq 0 && "${xml_tests:-0}" -lt "${total_tests:-0}" ]]; then
    detail "the JUnit XML accounts for ${xml_tests:-0} test(s) while the sbt log reports ${total_tests:-0}, so reports were lost and the artifact this gate counts is incomplete"
    failed=1
  fi

  {
    printf '\n# suites snapshotted: %s\n' "${SNAPSHOT_JUNIT_COUNT:-0}"
    printf '# files snapshotted from target/test-reports: %s\n' "${SNAPSHOT_JUNIT_FILES:-0}"
    printf '# parity reports snapshotted: %s\n' "${SNAPSHOT_PARITY_COUNT:-0}"
    printf '# files snapshotted from target/parity-report: %s\n' "${SNAPSHOT_PARITY_FILES:-0}"
    printf '# tests reported by sbt: %s\n' "$total_tests"
    printf '# tests accounted for by the snapshotted JUnit XML: %s\n' "${xml_tests:-0}"
    printf '# ScalaTest reporter breakages in the sbt log: %s\n' "${reporter_breakages:-0}"
    printf '# runs whose reports the build completed from sbt test events: %s\n' "${promotions:-0}"
    if [[ "${xml_tests:-0}" -gt "${total_tests:-0}" ]]; then
      printf '# NOTE: the XML accounts for %s more test(s) than the sbt log.\n' \
        "$((${xml_tests:-0} - ${total_tests:-0}))"
      printf '#       The XML is the complete account of the two: the framework summary in the\n'
      printf '#       log is printed from the events that reached the ScalaTest reporter.\n'
    fi
  } >>"$EV"

  if [[ "$failed" -eq 0 ]]; then
    detail "both modules compiled and every spec passed (${xml_tests:-0} tests in ${SNAPSHOT_JUNIT_COUNT} suite reports, ${total_tests} reported by sbt)"
  fi
  return "$failed"
}

#=============================================================================
# Row 2 - Gate 2 / Rule 1: dependency purity.
#
# Four checks, all of which must find nothing, on both Compile and Test:
#   1. the four dependency trees, grepped for guava, joda and the UNSUFFIXED
#      Maven coordinate `com.opengamma.strata:strata-collect:` - the Java
#      artefact. The in-build Scala project appears as `strata-collect_2.13`
#      and must not be confused with it.
#   2. the exported full classpaths, grepped for guava, joda and a VERSIONED
#      strata-collect jar. The Scala module contributes a `classes` directory,
#      never a jar, which is exactly what distinguishes the two.
#   3. each exported classpath value on its own, for a class name provided by
#      more than one entry - two artefacts that publish the same name leave
#      classpath order deciding which implementation a JVM links against.
#   4. the build definition and both module trees, grepped for the Java
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
    command_line 'sbt -batch "strata-basics/Compile/dependencyTree" "strata-basics/Test/dependencyTree" "strata-collect/Compile/dependencyTree" "strata-collect/Test/dependencyTree"'
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
    command_line 'sbt -batch "export strata-basics/Compile/fullClasspath" "export strata-basics/Test/fullClasspath"'
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

  # -- 3. duplicate classes on each exported classpath --------------------
  #
  # The same log, read a second time for a different property: not WHICH
  # artefacts are present but whether any two of them publish the same class
  # name. Each exported value is audited separately - the log carries four,
  # because strata-basics aggregates strata-collect - so the verdict belongs
  # to one classpath instead of to a pool in which no single JVM's view
  # exists. Load order is not a declared property of the build, so "the right
  # one happens to be first" is not an invariant; "no name has two providers"
  # is, and byte-different providers are reported as such because those are
  # the ones that change behaviour or fail linkage.
  local duplicates_report="$AUDIT_DIR/gate02-duplicate-classes.txt"
  local duplicates_relative="${duplicates_report#"$ROOT"/}"
  printf '## no class name is provided by two entries of one exported classpath\n' >>"$EV"
  printf '# report: %s\n' "$duplicates_relative" >>"$EV"
  if ! safe_truncate "$duplicates_report"; then
    detail "the duplicate-class report could not be started"
    failed=1
  elif ! python3 - "$duplicates_report" "$cp_log" <<'PY' >>"$EV" 2>&1; then
import collections
import hashlib
import os
import sys
import zipfile

report_path, log_path = sys.argv[1], sys.argv[2]

# Every classpath is expected to provide this one, from scala-library. It is
# the per-classpath positive control: an enumeration that produced nothing -
# because the value was misparsed, or every entry was unreadable - would
# otherwise report "no duplicates" and read as a clean classpath.
CONTROL_CLASS = "scala/Option.class"


def digest(payload):
    """The identity of a class file's bytes, for telling a duplicate that is a
    copy from one that is a different implementation."""
    return hashlib.sha256(payload).hexdigest()


def is_classpath(line):
    """`export` prints each value unprefixed, preceded by its own unprefixed
    key header; sbt's own chatter is `[...]`-prefixed and dropped before this.
    A value is an unprefixed line whose every separated field is an absolute
    path, which no key header is."""
    fields = [field for field in line.split(os.pathsep) if field]
    return bool(fields) and all(field.startswith("/") for field in fields)


def classes_in(entry):
    """Every `*.class` one classpath entry provides, mapped to the digest of
    its bytes. A jar is read as a zip and a directory is walked; an entry that
    is neither - an absent output directory, say - provides nothing."""
    found = {}
    if os.path.isdir(entry):
        for directory, _subdirectories, files in os.walk(entry):
            for name in files:
                if not name.endswith(".class"):
                    continue
                path = os.path.join(directory, name)
                with open(path, "rb") as handle:
                    found[os.path.relpath(path, entry).replace(os.sep, "/")] = digest(handle.read())
        return found
    if zipfile.is_zipfile(entry):
        with zipfile.ZipFile(entry) as archive:
            for info in archive.infolist():
                if info.filename.endswith(".class"):
                    found[info.filename] = digest(archive.read(info))
    return found


def provider_index(entry_classes):
    """class name -> [(entry, digest)], one pair per entry that provides it."""
    index = collections.defaultdict(list)
    for entry, classes in entry_classes.items():
        for name, sha in classes.items():
            index[name].append((entry, sha))
    return index


def duplicates(index):
    """The names more than one entry provides, and the subset whose providers
    disagree on the bytes."""
    shared = {name: providers for name, providers in index.items() if len(providers) > 1}
    divergent = {
        name: providers
        for name, providers in shared.items()
        if len({sha for _, sha in providers}) > 1
    }
    return shared, divergent


def self_check():
    """The detector, exercised on a fabricated index before any real classpath
    is judged: one name provided twice with different bytes, one provided
    twice with identical bytes, one provided once. A detector that does not
    report exactly the first two, and the byte difference of the first, would
    report a duplicated classpath as clean - so its own failure fails the row.
    Returns the discrepancy, or an empty string."""
    fixture = provider_index({
        "/fixture/first.jar": {
            "fixture/Divergent.class": digest(b"fixture-divergent-first"),
            "fixture/Identical.class": digest(b"fixture-identical"),
            "fixture/Unique.class": digest(b"fixture-unique"),
        },
        "/fixture/second.jar": {
            "fixture/Divergent.class": digest(b"fixture-divergent-second"),
            "fixture/Identical.class": digest(b"fixture-identical"),
        },
    })
    shared, divergent = duplicates(fixture)
    expected_shared = ["fixture/Divergent.class", "fixture/Identical.class"]
    expected_divergent = ["fixture/Divergent.class"]
    if sorted(shared) != expected_shared or sorted(divergent) != expected_divergent:
        return (
            f"the duplicate-class detector reported duplicated={sorted(shared)} "
            f"byte-different={sorted(divergent)} on its own fixture, expected "
            f"duplicated={expected_shared} byte-different={expected_divergent}")
    return ""


problems = []
report = []

discrepancy = self_check()
if discrepancy:
    problems.append(discrepancy)
report.append(
    "detector self-check: "
    + (discrepancy
       or "the fabricated fixture's byte-different and byte-identical duplicates were both "
          "reported, and its unique name was not"))

# The values, each attributed to the key header printed above it.
classpaths = []
label = ""
try:
    with open(log_path, encoding="utf-8", errors="replace") as handle:
        for raw in handle:
            text = raw.rstrip("\n").strip()
            if not text or text.startswith("["):
                continue
            if is_classpath(text):
                classpaths.append((label or f"classpath #{len(classpaths) + 1}", text))
                label = ""
            else:
                label = text
except OSError as error:
    problems.append(f"the exported classpath log {log_path} could not be read: {error}")

if not classpaths:
    problems.append(
        f"no exported classpath value could be parsed out of {log_path}, so no classpath was "
        "audited for duplicate classes")

total_class_files = 0
total_duplicated = 0
for name, value in classpaths:
    entries = [entry for entry in value.split(os.pathsep) if entry]
    entry_classes = {}
    absent = []
    for entry in entries:
        if not os.path.exists(entry):
            absent.append(entry)
            continue
        try:
            entry_classes[entry] = classes_in(entry)
        except (OSError, zipfile.BadZipFile) as error:
            problems.append(f"{name}: classpath entry {entry} could not be read: {error}")
    index = provider_index(entry_classes)
    shared, divergent = duplicates(index)
    class_files = sum(len(classes) for classes in entry_classes.values())
    total_class_files += class_files
    total_duplicated += len(shared)

    report.append(
        f"{name}: {len(entries)} entry(ies), {len(index)} class name(s), {class_files} class "
        f"file(s), {len(shared)} duplicated name(s), {len(divergent)} of them byte-different")
    for entry in absent:
        report.append(f"    entry absent on disk, provides nothing: {entry}")
    for duplicated in sorted(shared):
        kind = "BYTE-DIFFERENT" if duplicated in divergent else "byte-identical"
        report.append(f"    {kind}: {duplicated}")
        for entry, sha in shared[duplicated]:
            report.append(f"        {os.path.basename(entry.rstrip('/')) or entry} [{sha[:16]}] {entry}")

    if class_files == 0:
        problems.append(
            f"{name}: no class file was enumerated from any of its {len(entries)} entries, so "
            "this classpath was not audited")
    elif CONTROL_CLASS not in index:
        problems.append(
            f"{name}: positive control failed - {CONTROL_CLASS} is absent from the "
            f"{class_files} class file(s) enumerated here, so this audit is not reading what "
            "the classpath carries")
    if shared:
        problems.append(
            f"{name}: {len(shared)} class name(s) provided by more than one entry, "
            f"{len(divergent)} of them byte-different")

if total_class_files == 0:
    problems.append(
        "no class file was enumerated from any classpath, so this check measured nothing")

report.append(
    f"TOTAL: classpaths={len(classpaths)} class-files={total_class_files} "
    f"duplicated-names={total_duplicated} problems={len(problems)}")

try:
    with open(report_path, "w", encoding="utf-8") as handle:
        handle.write("\n".join(report) + "\n")
        for problem in problems:
            handle.write(f"PROBLEM: {problem}\n")
except OSError as error:
    problems.append(f"the duplicate-class report {report_path} could not be written: {error}")

print("\n".join(report))
for problem in problems:
    print(f"PROBLEM: {problem}")
sys.exit(1 if problems else 0)
PY
    local duplicate_detail
    duplicate_detail="$(awk '/^PROBLEM: / {
           sub(/^PROBLEM: /, "")
           printf "%s%s", (reported++ ? "; " : ""), $0
         }
         END { if (reported) printf "\n" }' "$duplicates_report")"
    detail "duplicate-class audit: ${duplicate_detail:-see $duplicates_relative}"
    failed=1
  fi
  printf '\n' >>"$EV"

  # -- 4. build definition and module sources -----------------------------
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
#     root project IS strata-basics). EVERY id it prints is parsed, whatever
#     it is called, and the complete set is compared against a literal
#     expectation rather than a count: an id the parse cannot see is a
#     project this row cannot reject.
#   * the Compile internal dependency classpath of strata-basics carries
#     strata-collect's `classes`, and the Test one carries BOTH that
#     `classes` directory and, additionally, its `test-classes` - on one and
#     the same classpath value: that is the proof of the directed edge
#     `"compile->compile;test->test"`, and neither half of it is optional.
#   * zero `.java` files under either module or the build definition.
#   * every project the build actually has, audited where it keeps its files:
#     its base directory and its Compile and Test source roots must resolve
#     inside this checkout and must hold no `.java` file. The two `find` roots
#     of the check above are the two directories the build is SUPPOSED to
#     consist of, so on their own they say nothing about a project rooted
#     somewhere else.
#   * the file-extension histogram, reported.
#   * each project's unmanaged source directories hold only its own
#     src/main/scala.
#=============================================================================

# sbt_show_values <log> - prints every element of the `show` values in a log,
# one per line. Every key it is used on is `File`- or `Seq[File]`-valued, so a
# value is an absolute path and anything else in the log is not a value: sbt's
# own `lintUnused` warning prints bulleted lines of key names in exactly the
# shape a bulleted element has, and reading one of those as a directory would
# put a key name where a path belongs.
#
# sbt prints a value in one of three shapes, and all three are parsed
# structurally - by line and by delimiter - never by splitting on whitespace.
# A checkout path may legitimately contain a space (a developer clone under
# "My Documents" is enough), and a parser that tokenised on whitespace would
# find no directory at all and fail a row for a reason that has nothing to do
# with the build:
#   [info] <TAB>List(/a/src/main/scala, /b/src/main/scala)   a collection
#   [info] * /a/src/main/scala                               a bulleted element
#   [info] <TAB>/a                                           a single File
# The third shape is recognised by the leading separator of an absolute path,
# which sbt's own chatter never has: every line it logs around a value begins
# with a word ("loading settings for project", "set current project to",
# "Total time"). A `File`-valued key such as `baseDirectory` is printed that
# way, so a parser that read only the first two shapes would silently return
# nothing for it.
# The only sequence a path may therefore not contain is the ", " that
# separates the elements of a collection, which is noted here because nothing
# in the parse can distinguish it.
sbt_show_values() {
  awk '
       function trim(s) {
         gsub(/^[[:space:]]+|[[:space:]]+$/, "", s)
         return s
       }
       function emit(value) {
         value = trim(value)
         # An absolute path, which is what every value of these keys is.
         if (value ~ /^\//) print value
       }
       {
         text = $0
         sub(/^\[(info|warn|error|success|debug)\][[:space:]]?/, "", text)
         text = trim(text)

         # One element per line after a bullet. `emit` is what refuses a
         # bulleted line that is not a path.
         if (text ~ /^\*[[:space:]]/) {
           emit(substr(text, 2))
           next
         }
         # A collection literal holding the elements.
         if (text ~ /^(List|Vector|Seq|ArrayBuffer|ArraySeq)\(.*\)$/) {
           inner = text
           sub(/^[A-Za-z]+\(/, "", inner)
           sub(/\)$/, "", inner)
           if (trim(inner) == "") next
           count = split(inner, elements, ", ")
           for (i = 1; i <= count; i++) emit(elements[i])
           next
         }
         # A single absolute path, which is how a File-valued key is printed.
         if (text ~ /^\//) {
           emit(text)
         }
       }' "$1"
}

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

  # EVERY id the build lists, under whatever name. There is no name filter and
  # no character class: an id the parse declines to see cannot be rejected, so
  # a `helper`, a `tools_2` or a `zzé` would sit in the build unmeasured while
  # the comparison below still matched the two expected lines. An entry is
  # therefore any line of the project block that is a single token, which is a
  # superset of every id sbt accepts - its own id parser admits Unicode
  # letters and digits with `_` and `-`, and no whitespace at all - and each
  # is taken WHOLE, never as a prefix, so `strata-basics-it` is an id of its
  # own rather than another spelling of `strata-basics`.
  #
  # The entries are those of the `In <build uri>:` block `sbt projects`
  # prints, which is what distinguishes them from the `loading settings for
  # project strata-basics` and `set current project to strata-basics` lines
  # that precede it. The current project carries a `*` bullet and the others
  # none; both shapes are accepted. sbt 1.13 emits a TAB after `[info]`, so
  # the prefix is stripped rather than matched with a space. A line inside the
  # block that is NOT a single token cannot be an id - sbt's own timing and
  # status lines are of that shape - so it closes the block and is recorded as
  # the line that closed it, which keeps the parse readable in the evidence.
  # awk exits 0 when nothing matches, so an empty result reaches the
  # comparison below instead of failing the pipeline, and is reported in its
  # own right: a parse that saw no id has measured no project set.
  local block_end="$AUDIT_DIR/gate02a-project-block-end.txt"
  awk -v block_end="$block_end" '
       function trim(s) {
         gsub(/^[[:space:]]+|[[:space:]]+$/, "", s)
         return s
       }
       {
         line = $0
         sub(/^\[(info|warn|error|success|debug)\][[:space:]]?/, "", line)
         line = trim(line)

         if (line ~ /^In [A-Za-z][A-Za-z0-9+.-]*:/) {
           inside = 1
           printf "block opened by: %s\n", line > block_end
           next
         }
         if (!inside) next

         entry = line
         sub(/^\*[[:space:]]*/, "", entry)
         if (entry != "" && entry !~ /[[:space:]]/) {
           print entry
           next
         }
         inside = 0
         printf "block closed by: %s\n", (line == "" ? "(a blank line)" : line) > block_end
       }
       END {
         # The list is the last thing in the log when nothing follows it, so
         # the block closes at end of input. Recording that says so, rather
         # than leaving the evidence with an opening and no close - which a
         # reader cannot tell apart from a close that went unrecorded.
         if (inside) {
           printf "block closed by: (the end of the output)\n" > block_end
         }
       }' "$projects_log" | sort -u >"$ids"
  printf 'strata-basics\nstrata-collect\n' >"$expected_ids"

  {
    command_line 'sbt -batch projects'
    printf '# sbt exit status: %s\n' "$rc"
    printf '# project ids found (every single-token entry of the project block):\n'
    cat "$ids"
    printf '# expected:\n'
    cat "$expected_ids"
    printf '# how the project block was delimited:\n'
    if [[ -s "$block_end" ]]; then
      sed 's/^/#   /' "$block_end"
    else
      printf '#   (no project block was found in the output)\n'
    fi
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "sbt projects failed with status $rc"
    failed=1
  fi
  local id_count
  id_count="$(awk 'NF { n++ } END { print n + 0 }' "$ids")"
  if [[ "${id_count:-0}" -lt 1 ]]; then
    detail "no project id could be parsed out of sbt projects, so the build's project set is unmeasured"
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
    printf '\n'
    command_line 'sbt -batch "show strata-basics/Compile/internalDependencyClasspath" "show strata-basics/Test/internalDependencyClasspath"'
    printf '# sbt exit status: %s\n' "$rc"
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
    printf '\n'
    command_line 'find strata-collect strata-basics project -name "*.java"'
    printf '# find exit status: %s\n' "$find_rc"
    printf '%s\n' "${java_count:-0}"
    cat "$java_list"
  } >>"$EV"
  if [[ "${java_count:-0}" -ne 0 ]]; then
    detail "${java_count} .java file(s) inside the sbt build"
    failed=1
  fi

  # -- every discovered project, audited where it keeps its files ----------
  #
  # The three roots the `find` above scans are the directories this build is
  # supposed to consist of; they are not a statement about the projects the
  # build actually has. A project based elsewhere - or one pointed at a source
  # root outside its own directory - would be audited by neither that find nor
  # the two-project source-directory comparison below. So each id parsed from
  # `sbt projects` is asked where it lives, and every directory it names is
  # held to two things: it resolves inside this checkout, and it holds no
  # `.java` file. Paired with the closed id set above, that covers what the
  # build contains as well as what it is called.
  #
  # The base directories and the source roots are asked for SEPARATELY, so
  # that each answer keeps the kind of key it came from. That distinction is
  # the whole point of asking twice: the root project legitimately bases at
  # the checkout root, which holds the Maven tree this build has nothing to do
  # with, so a base directory equal to the root is exempt from the tree scan
  # (its own content is covered by its source roots and by the build
  # directories scanned above) - while a SOURCE ROOT equal to the checkout
  # root is a project reaching over the Java tree and is scanned like any
  # other, which is exactly what makes it fail. Merging the two answers into
  # one list would make those two cases indistinguishable.
  #
  # A source root that does not exist is normal (sbt reports the configured
  # path whether or not it was created) and is recorded as absent; a directory
  # that exists but points outside the checkout, and any `.java` file under
  # any of them, fails the row.
  local -a base_show=() roots_show=()
  local project_id
  while IFS= read -r project_id; do
    [[ -n "$project_id" ]] || continue
    base_show+=("show $project_id/baseDirectory")
    roots_show+=("show $project_id/Compile/unmanagedSourceDirectories")
    roots_show+=("show $project_id/Test/unmanagedSourceDirectories")
  done <"$ids"

  local base_dirs="$AUDIT_DIR/gate02a-project-base-directories.txt"
  local source_roots="$AUDIT_DIR/gate02a-project-source-roots.txt"
  local project_audit="$AUDIT_DIR/gate02a-project-audit.txt"
  if ! safe_truncate "$base_dirs" || ! safe_truncate "$source_roots" ||
    ! safe_truncate "$project_audit"; then
    detail "the Gate 2a per-project audit files could not be started"
    failed=1
  fi

  if [[ "${#base_show[@]}" -eq 0 ]]; then
    printf '\n# no project id was parsed, so no project could be audited\n' >>"$EV"
    detail "no project could be audited, because no id was parsed"
    failed=1
  else
    local base_rc=0 roots_rc=0
    run_sbt gate02a-project-base-directories "${base_show[@]}" || base_rc=$?
    sbt_show_values "$SBT_LOG" | sort -u >"$base_dirs"
    run_sbt gate02a-project-source-roots "${roots_show[@]}" || roots_rc=$?
    sbt_show_values "$SBT_LOG" | sort -u >"$source_roots"

    local base_count roots_count
    base_count="$(awk 'NF { n++ } END { print n + 0 }' "$base_dirs")"
    roots_count="$(awk 'NF { n++ } END { print n + 0 }' "$source_roots")"

    local audited_dirs=0 absent_dirs=0 outside_dirs=0 java_in_projects=0 root_based=0
    local kind list project_dir canonical dir_java dir_find_rc dir_java_count
    for kind in base-directory source-root; do
      if [[ "$kind" == "base-directory" ]]; then
        list="$base_dirs"
      else
        list="$source_roots"
      fi
      while IFS= read -r project_dir; do
        [[ -n "$project_dir" ]] || continue
        audited_dirs=$((audited_dirs + 1))
        if [[ "$project_dir" != "$ROOT" && "$project_dir" != "$ROOT"/* ]]; then
          printf '%s\t%s\tOUTSIDE the checkout\n' "$kind" "$project_dir" >>"$project_audit"
          outside_dirs=$((outside_dirs + 1))
          continue
        fi
        if [[ ! -d "$project_dir" ]]; then
          printf '%s\t%s\tabsent (a configured path that was never created)\n' \
            "$kind" "$project_dir" >>"$project_audit"
          absent_dirs=$((absent_dirs + 1))
          continue
        fi
        canonical="$(canonical_dir "$project_dir")"
        if [[ -z "$canonical" ]] || ! is_inside_root "$canonical"; then
          printf '%s\t%s\tOUTSIDE the checkout once resolved (%s)\n' \
            "$kind" "$project_dir" "${canonical:-unresolvable}" >>"$project_audit"
          outside_dirs=$((outside_dirs + 1))
          continue
        fi
        # The exemption is for a BASE directory only, and only for the
        # checkout root itself. A source root there is scanned, and the Java
        # tree it reaches over is what fails the row.
        if [[ "$kind" == "base-directory" && "$canonical" == "$ROOT_REAL" ]]; then
          printf '%s\t%s\tthe checkout root, where the root project bases: inside the checkout, its own content covered by its source roots and by the build directories scanned above\n' \
            "$kind" "$project_dir" >>"$project_audit"
          root_based=$((root_based + 1))
          continue
        fi
        dir_find_rc=0
        dir_java="$(command find "$canonical" -name '*.java' -print 2>>"$EV")" || dir_find_rc=$?
        if [[ "$dir_find_rc" -ne 0 ]]; then
          printf '%s\t%s\tSCAN FAILED (find exited %s)\n' "$kind" "$project_dir" "$dir_find_rc" \
            >>"$project_audit"
          detail "the .java scan of $project_dir exited $dir_find_rc, so its result is not authoritative"
          failed=1
          continue
        fi
        if [[ -n "$dir_java" ]]; then
          dir_java_count="$(printf '%s\n' "$dir_java" | awk 'NF { n++ } END { print n + 0 }')"
          printf '%s\t%s\t%s .java file(s):\n%s\n' \
            "$kind" "$project_dir" "$dir_java_count" "$dir_java" >>"$project_audit"
          java_in_projects=$((java_in_projects + dir_java_count))
          continue
        fi
        printf '%s\t%s\tno .java file\n' "$kind" "$project_dir" >>"$project_audit"
      done <"$list"
    done

    {
      printf '\n'
      command_line "sbt -batch$(printf ' \"%s\"' "${base_show[@]}")"
      printf '# sbt exit status: %s\n' "$base_rc"
      command_line "sbt -batch$(printf ' \"%s\"' "${roots_show[@]}")"
      printf '# sbt exit status: %s\n' "$roots_rc"
      printf '# %s discovered project(s); %s base directory(ies) and %s source root(s) parsed\n' \
        "${id_count:-0}" "$base_count" "$roots_count"
      printf '# per-directory audit (%s absent, %s the checkout root as a base directory,\n' \
        "$absent_dirs" "$root_based"
      printf '# %s outside the checkout, %s .java file(s)):\n' "$outside_dirs" "$java_in_projects"
      cat "$project_audit"
    } >>"$EV"

    if [[ "$base_rc" -ne 0 ]]; then
      detail "show baseDirectory over the discovered projects failed with status $base_rc"
      failed=1
    fi
    if [[ "$roots_rc" -ne 0 ]]; then
      detail "show Compile/Test unmanagedSourceDirectories over the discovered projects failed with status $roots_rc"
      failed=1
    fi
    # Proof that the two answers were actually parsed: every project has a
    # base directory, so there is at least one value per project, and the
    # build root is among them because sbt always bases a project there.
    if [[ "${base_count:-0}" -lt "${id_count:-1}" ]]; then
      detail "only ${base_count:-0} base directory(ies) parsed for ${id_count:-0} project(s), so the audit is incomplete"
      failed=1
    fi
    if ! grep -qxF "$ROOT" "$base_dirs"; then
      detail "the checkout root is not among the parsed base directories, so the base-directory parse is not trustworthy"
      failed=1
    fi
    if [[ "${roots_count:-0}" -lt "${id_count:-1}" ]]; then
      detail "only ${roots_count:-0} source root(s) parsed for ${id_count:-0} project(s), so the audit is incomplete"
      failed=1
    fi
    if [[ "$audited_dirs" -lt 1 ]]; then
      detail "no directory could be parsed for the discovered project(s), so none was audited"
      failed=1
    fi
    if [[ "$outside_dirs" -ne 0 ]]; then
      detail "$outside_dirs project directory(ies) lie outside this checkout"
      failed=1
    fi
    if [[ "$java_in_projects" -ne 0 ]]; then
      detail "$java_in_projects .java file(s) under the discovered projects' own directories"
      failed=1
    fi
    if ! add_appendix "Gate 2a - every discovered project, audited where it keeps its files" \
      <"$project_audit"; then
      detail "the per-project audit could not be appended to the report"
      failed=1
    fi
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
  if ! safe_truncate "$dirs"; then
    detail "the Gate 2a source-directory list could not be started"
    failed=1
  fi
  sbt_show_values "$dirs_log" | sort -u >"$dirs"

  {
    printf '\n'
    command_line 'sbt -batch "show strata-basics/Compile/unmanagedSourceDirectories" "show strata-collect/Compile/unmanagedSourceDirectories"'
    printf '# sbt exit status: %s\n' "$rc"
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
    detail "exactly two Scala-only projects and no third, 0 .java files here or under any project's own roots, strata-basics -> strata-collect edge proven"
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
    command_line 'sbt -batch "testOnly *ParitySpec"'
    printf '# sbt exit status: %s\n' "$rc"
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
# this block is published as the authoritative record of why 40 public types
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
EXCLUDED com.opengamma.strata.basics.ReferenceDataType typeclass, helper or effect rather than data
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
    command_line 'sbt -batch "testOnly *JsonRoundTripSpec *CodecsSpec"'
    printf '# sbt exit status: %s\n' "$rc"
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
# Row 6 - Gate 5 / Rule 3: no `var` in domain code, and no aliasing of the
# numeric backing arrays.
#
#   grep -rnw var strata-collect/src/main/scala strata-basics/src/main/scala
#
# must be empty. Whole-word match, and there are no file exclusions: every
# occurrence of the token in either module's main sources fails this row.
#
# Rule 3 is immutability, of which the absence of `var` is one half and the
# unreachability of a value's storage is the other, so both are measured here.
# The second half is a BYTECODE assertion, because a source-level one cannot
# state it: `private[collect]` restricts a member in the source and the
# compiler emits it as a public method regardless, so `DoubleArray` and
# `DoubleMatrix` enforce immutability by copying in their sole constructor and
# by publishing no member that hands out what they hold. Two things are
# therefore asserted over the four class files - the two types and their
# companion objects:
#
#   * no method name contains `Unsafe`, which is what the Java original's two
#     aliasing members were called and the name any reintroduction would most
#     likely carry;
#   * no PUBLIC method returns `[D` or `[[D` except the copying accessors -
#     `toArray`, `rowArray`, `columnArray` and the matrix's deep copy, which
#     the compiler emits under the mangled name
#     `com$opengamma$strata$collect$array$DoubleMatrix$$deepClone` because the
#     class reaches the companion's private helper. That one is a copier of
#     the argument it is handed and reads no field of any instance, so it is
#     no route into a value; it is named in the allowed list rather than
#     excluded by a pattern, so a differently named method returning an array
#     fails this row.
#
# The return type is read from javap's declaration line, whose Java syntax
# renders the two descriptors as `double[]` and `double[][]` before the method
# name. A parameter of either type is not a finding, which is why the match is
# anchored on the text before the name and not on the line as a whole.
#=============================================================================

# The copying accessors, which are the only public members of the two numeric
# types permitted to answer with a primitive array.
ARRAY_RETURN_ALLOWED='toArray rowArray columnArray com$opengamma$strata$collect$array$DoubleMatrix$$deepClone'

row_06_no_var() {
  new_evidence gate05-no-var.txt
  local failed=0

  printf '## Gate 5 / Rule 3 - no `var` in either module main sources\n\n' >>"$EV"
  if assert_no_match "var in main sources" "$EV" -rnw var "$COLLECT_MAIN" "$BASICS_MAIN"; then
    detail "no occurrence of the token in $COLLECT_MAIN or $BASICS_MAIN"
  else
    failed=1
  fi

  # -- the other half of Rule 3: the storage of the numeric types ----------
  {
    printf '## Gate 5 / Rule 3 - no bytecode member aliases a numeric backing array\n'
    command_line 'javap -s -p over DoubleArray.class, DoubleArray$.class, DoubleMatrix.class and DoubleMatrix$.class, reading every member signature for a primitive-array return outside the copying accessors'
    printf '# allowed public array returns: %s\n\n' "$ARRAY_RETURN_ALLOWED"
  } >>"$EV"

  local class_files=(
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray\$.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleMatrix.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleMatrix\$.class"
  )
  local aliasing="$AUDIT_DIR/gate05-array-aliasing.txt"
  : >"$aliasing"

  local class_file dump stem examined=0
  for class_file in "${class_files[@]}"; do
    if ! require_file "$class_file" "numeric class file for the aliasing check"; then
      printf '# MISSING: %s\n' "$class_file" >>"$EV"
      failed=1
      continue
    fi
    stem="$(basename "$class_file" .class)"
    dump="$AUDIT_DIR/gate05-members-${stem//\$/-object}.txt"
    if ! javap -p "$class_file" >"$dump" 2>>"$EV"; then
      detail "javap failed on $class_file"
      failed=1
      continue
    fi
    examined=$((examined + 1))
    printf '# %-78s -> %s\n' "${class_file#"$ROOT"/}" "${dump#"$ROOT"/}" >>"$EV"
    # One line per finding: the class, the kind of violation and the whole
    # declaration javap printed, so a failure names the member to look at.
    awk -v subject="$stem" -v allowed="$ARRAY_RETURN_ALLOWED" '
      BEGIN { count = split(allowed, names, " "); for (i = 1; i <= count; i++) { permitted[names[i]] = 1 } }
      # a member declaration, which javap indents by exactly two spaces
      /^  [^ ]/ {
        declaration = $0
        sub(/^[ \t]+/, "", declaration)
        if (declaration !~ /\(/) { next }                 # a field, not a method
        signature = declaration
        sub(/\(.*$/, "", signature)                       # drop the parameters
        name = signature
        sub(/^.*[ .]/, "", name)                          # the simple method name
        if (name ~ /Unsafe/) {
          print subject ": method name contains Unsafe: " declaration
          next
        }
        if (declaration !~ /^public/) { next }
        if (signature ~ /double\[\]\[\][ \t]/ || signature ~ /double\[\][ \t]/) {
          if (!(name in permitted)) {
            print subject ": public method returns a primitive array: " declaration
          }
        }
      }
    ' "$dump" >>"$aliasing"
  done

  if [[ "$examined" -ne "${#class_files[@]}" ]]; then
    detail "$examined of ${#class_files[@]} numeric class files could be disassembled"
    failed=1
  fi

  local findings
  findings="$(awk 'NF { n++ } END { print n + 0 }' "$aliasing")"
  {
    printf '\n# aliasing findings: %s\n' "$findings"
    cat "$aliasing"
  } >>"$EV"
  add_appendix "Gate 5 - members of the numeric types that answer with an array" <"$aliasing"

  if [[ "$findings" -ne 0 ]]; then
    detail "$findings bytecode member(s) of DoubleArray/DoubleMatrix alias a backing array"
    failed=1
  elif [[ "$failed" -eq 0 ]]; then
    detail "no member of the four numeric class files is named Unsafe or returns a primitive array outside the copying accessors"
  fi

  return "$failed"
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
    command_line 'javap -c -p over DoubleArray.class, DoubleArray$.class, DoubleMatrix.class, DoubleMatrix$.class and DoubleArrayMath$.class, restricted to the bodies of the hot methods and their transitive call graph, counting scala/runtime/BoxesRunTime, java/lang/Double.valueOf and Double.doubleValue calls'
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
#                  inner loops over `Array[Double]`. The CONSTRUCTOR of each
#                  of the two numeric classes is a root here too, under the
#                  simple class name, which is the name javap's declaration
#                  line gives it: each class produces the storage it keeps in
#                  its constructor and applies the operation's own in-place
#                  loop to that storage there, so those loops are reached
#                  from the constructor and from nowhere else. The graph
#                  cannot walk to it - javap prints a constructor call as
#                  `"<init>"`, which matches no declaration - so naming it as
#                  a root is what puts its body, and the loops it reaches,
#                  inside the audit.
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
                        "with", "get", "contains", "indexOf", "lastIndexOf", "forEach", "toArray",
                        "DoubleArray"],
        "helpers": ["plusInto", "minusInto", "scaledInto", "mapInto", "mapWithIndexInto",
                    "plusEachInto", "minusEachInto", "multipliedByEachInto", "dividedByEachInto",
                    "combineEachInto", "ternaryFoldFrom", "minFrom", "maxFrom", "sumFrom",
                    "reduceFrom", "concatArray", "firstIndexOfFrom", "lastIndexOfFrom",
                    "forEachFrom", "rewritten"],
        "floor": 56,
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
        "elementwise": ["row", "column", "get", "with", "forEach", "toArray", "DoubleMatrix"],
        "helpers": ["reduceFrom", "totalFrom", "forEachFrom", "columnCopy", "fillColumn",
                    "rewritten", "scaledInto", "mapInto", "mapWithIndexInto", "plusEachInto",
                    "minusEachInto", "combineEachInto"],
        "floor": 37,
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
#   * no throw EXPRESSION of any spelling outside ArgCheck.scala, which is
#     where the documented fail-fast invariant throws live. The
#     specification's `throw new` grep is one spelling of the construct, so
#     it is reported and a lexical scan decides the row;
#   * the four specs that prove the failable surface green.
#=============================================================================

# rule5_throw_scan <allow-list file | -> <source root>...
#
# Reports every throw EXPRESSION in the Scala sources below the roots, and
# fails when one of them is outside the allow-list (`-` allows none).
#
#   0  every throw is in an allowed file
#   1  at least one throw is not
#   2  the scan could not be carried out: a file it could not read, or no
#      Scala source below the roots at all
#
# Text matching is not enough for this check. `throw new X(...)`,
# `throw(new X(...))`, a `throw` whose `new` sits on the next line and
# `throw t` on a caught exception are all the same construct, and a pattern
# written for one of them passes the others through. So each file is first
# reduced to its code: comments (line and nested block), string and character
# literals and backticked identifiers are replaced by spaces, newlines are
# preserved so every line number still points at the source line, and every
# remaining `throw` KEYWORD token is a hit. Blanking the comments is
# load-bearing rather than tidiness - the two module trees use the word
# "throw" in ten scaladoc sentences, and a scan that read prose would fail
# this row for documenting the design.
#
# The `${...}` block of an INTERPOLATED string is not literal text but
# executable Scala, so `s"${throw new X}"` is a throw and is kept as code:
# the interpolator is recognised from the identifier character in front of
# the quote, the block's closing brace is found by counting braces while
# skipping the literals and comments inside it, and its content is reduced by
# this same function. Every replacement is length-preserving, so recursing
# into a block leaves the line numbering of the rest of the file intact.
#
# The walk is fail-closed: a root that is not a directory, a directory the
# walk cannot read and a file that cannot be decoded are each reported and
# exit 2, rather than being passed over while another root keeps the file
# count non-zero.
rule5_throw_scan() {
  python3 - "$@" <<'PY'
import os
import re
import sys

allowed_path = sys.argv[1]
roots = sys.argv[2:]

# `throw` as a token: not part of a longer identifier such as `throwaway`,
# `rethrow` or `throwIfEmpty`.
THROW = re.compile(r"(?<![A-Za-z0-9_$])throw(?![A-Za-z0-9_$])")
# The character in front of a quote when the quote opens an interpolated
# string: `s"..."`, `f"..."`, `raw"..."` or any custom interpolator.
INTERPOLATOR_CHAR = re.compile(r"[A-Za-z0-9_$]")


def closing_brace(text, open_index):
    """The index of the `}` that closes the `{` at open_index, or None.

    Braces are counted, and the literals and comments inside the block are
    skipped, so a brace or a quote written inside a nested string cannot end
    the block early.
    """
    depth = 0
    index = open_index
    length = len(text)
    while index < length:
        char = text[index]
        if char == "{":
            depth += 1
            index += 1
            continue
        if char == "}":
            depth -= 1
            if depth == 0:
                return index
            index += 1
            continue
        if text.startswith('"""', index):
            index += 3
            while index < length and not text.startswith('"""', index):
                index += 1
            index += 3
            continue
        if char == '"':
            index += 1
            while index < length and text[index] not in ('"', "\n"):
                index += 2 if text[index] == "\\" else 1
            index += 1
            continue
        if char == "/" and text.startswith("//", index):
            while index < length and text[index] != "\n":
                index += 1
            continue
        if char == "/" and text.startswith("/*", index):
            index += 2
            while index < length and not text.startswith("*/", index):
                index += 1
            index += 2
            continue
        index += 1
    return None


def blank_string(text, start, delimiter, interpolated, out):
    """Blanks one string literal, keeping its `${...}` blocks when it is
    interpolated. Returns the index just past the closing delimiter."""
    length = len(text)
    out.append(" " * len(delimiter))
    index = start + len(delimiter)
    while index < length:
        if delimiter == '"""':
            if text.startswith('"""', index):
                break
        else:
            if text[index] in ('"', "\n"):
                break
            if text[index] == "\\" and index + 1 < length:
                out.append("  ")
                index += 2
                continue
        if interpolated and text[index] == "$" and text.startswith("${", index):
            end = closing_brace(text, index + 1)
            if end is not None:
                # The `$`, `{` and `}` are punctuation; what is between them
                # is Scala, reduced by the same rules as the file around it.
                out.append("  ")
                out.append(blank_non_code(text[index + 2:end]))
                out.append(" ")
                index = end + 1
                continue
        out.append("\n" if text[index] == "\n" else " ")
        index += 1
    if delimiter == '"""':
        while index < length and text[index] == '"':
            out.append(" ")
            index += 1
    elif index < length and text[index] == '"':
        out.append(" ")
        index += 1
    return index


def blank_non_code(text):
    """Every comment, literal and backticked identifier replaced by spaces.

    Newlines survive and every replacement is the length of what it replaces,
    so the offset of a match still identifies the line it was written on, and
    no construct inside a comment or a string can be read as code - except
    the `${...}` block of an interpolated string, which is code.
    """
    out = []
    index = 0
    length = len(text)
    while index < length:
        char = text[index]
        following = text[index + 1] if index + 1 < length else ""
        if char == "/" and following == "/":
            while index < length and text[index] != "\n":
                out.append(" ")
                index += 1
            continue
        if char == "/" and following == "*":
            # Scala block comments nest, so the depth is tracked rather than
            # closing at the first `*/`.
            depth = 0
            while index < length:
                if text.startswith("/*", index):
                    depth += 1
                    out.append("  ")
                    index += 2
                    continue
                if text.startswith("*/", index):
                    depth -= 1
                    out.append("  ")
                    index += 2
                    if depth <= 0:
                        break
                    continue
                out.append("\n" if text[index] == "\n" else " ")
                index += 1
            continue
        if char == '"':
            interpolated = (index > 0
                            and INTERPOLATOR_CHAR.match(text[index - 1]) is not None)
            delimiter = '"""' if text.startswith('"""', index) else '"'
            index = blank_string(text, index, delimiter, interpolated, out)
            continue
        if char == "`":
            out.append(" ")
            index += 1
            while index < length and text[index] not in ("`", "\n"):
                out.append(" ")
                index += 1
            if index < length and text[index] == "`":
                out.append(" ")
                index += 1
            continue
        if char == "'":
            # A character literal is 'x' or '\x'; anything else beginning with
            # a quote is an operator or a symbol literal and is left alone.
            if index + 2 < length and text[index + 1] == "\\":
                end = index + 2
                while end < length and text[end] not in ("'", "\n"):
                    end += 1
                if end < length and text[end] == "'":
                    out.append(" " * (end - index + 1))
                    index = end + 1
                    continue
            elif index + 2 < length and text[index + 2] == "'":
                out.append("   ")
                index += 3
                continue
            out.append(char)
            index += 1
            continue
        out.append(char)
        index += 1
    return "".join(out)


allowed = set()
if allowed_path != "-":
    try:
        with open(allowed_path, encoding="utf-8") as handle:
            allowed = {line.strip() for line in handle if line.strip()}
    except OSError as error:
        print(f"# the allow-list could not be read: {error}")
        sys.exit(2)

hits = []
violations = []
unreadable = []
scanned = 0


def record_walk_error(error):
    """A directory the walk could not read is a gap in the scan, not a
    directory without Scala files."""
    unreadable.append(f"{getattr(error, 'filename', 'unknown path')}: {error}")


for root in roots:
    if not os.path.isdir(root):
        unreadable.append(f"{root}: not a directory, so it was not scanned")
        continue
    for directory, subdirectories, filenames in os.walk(root, onerror=record_walk_error):
        subdirectories.sort()
        for filename in sorted(filenames):
            if not filename.endswith(".scala"):
                continue
            path = os.path.join(directory, filename)
            scanned += 1
            try:
                with open(path, encoding="utf-8") as handle:
                    text = handle.read()
            except (OSError, UnicodeDecodeError) as error:
                unreadable.append(f"{path}: {error}")
                continue
            code = blank_non_code(text)
            source_lines = text.splitlines()
            for match in THROW.finditer(code):
                line_number = code.count("\n", 0, match.start()) + 1
                line_text = (source_lines[line_number - 1].strip()
                             if line_number <= len(source_lines) else "")
                record = f"{path}:{line_number}: {line_text}"
                hits.append(record)
                if path not in allowed:
                    violations.append(record)

print(f"# Scala source files scanned: {scanned}")
print(f"# allowed locations: {len(allowed)}")
for path in sorted(allowed):
    print(f"#   {path}")
print(f"# throw expressions found: {len(hits)}")
for record in hits:
    print(f"#   {record}")
print(f"# throw expressions outside the allowed locations: {len(violations)}")
for record in violations:
    print(f"#   VIOLATION {record}")
for problem in unreadable:
    print(f"#   UNREADABLE {problem}")

if unreadable:
    sys.exit(2)
if scanned < 1:
    print("# no Scala source file was scanned, so this scan measured nothing")
    sys.exit(2)
if violations:
    sys.exit(1)
sys.exit(0)
PY
}

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
    command_line "grep -rnE \"throw new\" $COLLECT_MAIN $BASICS_MAIN, excluding /ArgCheck.scala"
    printf '# grep exit status: %s\n' "$throw_rc"
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

  # -- every throw expression, whatever its spelling -----------------------
  #
  # The grep above is the specification's command and covers one spelling.
  # This scan is the one that decides the row, and ArgCheck.scala is named by
  # its exact path rather than by its file name, so a second file of that
  # name anywhere in either tree is a violation like any other.
  local allowed_throws="$AUDIT_DIR/gate05-throw-allowed.txt"
  printf '%s\n' "$COLLECT_MAIN/com/opengamma/strata/collect/ArgCheck.scala" >"$allowed_throws"
  local scan_out=""
  local scan_rc=0
  scan_out="$(rule5_throw_scan "$allowed_throws" "$COLLECT_MAIN" "$BASICS_MAIN" 2>&1)" || scan_rc=$?
  {
    printf '## every throw expression in either module, found lexically\n'
    command_line "rule5_throw_scan ${allowed_throws#"$ROOT"/} $COLLECT_MAIN $BASICS_MAIN"
    printf '# scan exit status: %s\n' "$scan_rc"
    printf '%s\n\n' "$scan_out"
  } >>"$EV"
  local outside_throws
  outside_throws="$(printf '%s\n' "$scan_out" |
    awk '/^# throw expressions outside the allowed locations: / { print $NF }')"
  case "$scan_rc" in
    0) ;;
    1)
      detail "${outside_throws:-one or more} throw expression(s) outside ArgCheck.scala, found lexically"
      failed=1
      ;;
    *)
      detail "the lexical throw scan could not be carried out (status $scan_rc)"
      failed=1
      ;;
  esac

  # -- the throw scan's own negative control -------------------------------
  #
  # Four spellings of a throw and seven decoys. Every line the scan must report
  # carries an `@throw-site` marker in a comment, and comments are precisely
  # what the scan blanks out before it looks for anything, so the marker
  # cannot help it find a site. The reported line set must equal the marked
  # line set exactly: a missing line means the scan does not recognise that
  # spelling and would pass a build that used it, and a surplus line means it
  # reads comments, strings or identifiers as code and would fail a build
  # that did not throw at all.
  #
  # The control lives under target/audit, never in a source tree: this script
  # writes nothing a gate scans.
  local control_dir="$AUDIT_DIR/rule5-throw-control"
  local control="$control_dir/ThrowFormsControl.scala"
  if ! ensure_output_dir "$control_dir" || ! safe_truncate "$control"; then
    detail "the throw-scan negative control could not be written"
    failed=1
  else
    cat >"$control" <<'CONTROL'
// A control for the Rule 5 throw scan: six spellings of one construct, and
// nine decoys that are not throws at all.
object ThrowFormsControl {
  val message: String = "a message"
  def plain(): Int = throw new IllegalStateException("the spelling the grep finds") // @throw-site
  def parenthesised(): Int = throw(new IllegalStateException("parenthesised")) // @throw-site
  def acrossLines(): Int =
    throw // @throw-site
      new IllegalStateException("the new sits on the next line")
  def rethrown(caught: Throwable): Int = throw caught // @throw-site
  // Inside an interpolation the braces hold code, not text.
  def inInterpolation: String = s"${throw new IllegalStateException(message)}" // @throw-site
  def inTripleInterpolation: String = s"""${throw(new IllegalStateException("nested"))}""" // @throw-site
  // A commented throw new RuntimeException is not a throw.
  val inString: String = "throw new RuntimeException"
  val interpolatedText: String = s"throw new RuntimeException $message"
  val notInterpolated: String = "${throw new RuntimeException}"
  val inTripleQuote: String = """throw new RuntimeException"""
  val inTripleQuoteBraces: String = """${throw new RuntimeException}"""
  val escapedQuote: String = "a \" then throw new RuntimeException"
  /* A block comment naming throw new RuntimeException
     over two lines, with a /* nested */ comment inside. */
  def throwaway(): Int = 0
  def rethrow(): Int = 0
}
CONTROL
    local expected_sites="$AUDIT_DIR/rule5-throw-control-expected.txt"
    local reported_sites="$AUDIT_DIR/rule5-throw-control-reported.txt"
    local control_out=""
    local control_rc=0
    grep -n '@throw-site' "$control" | cut -d: -f1 | sort -n >"$expected_sites"
    control_out="$(rule5_throw_scan - "$control_dir" 2>&1)" || control_rc=$?
    printf '%s\n' "$control_out" |
      awk '/^#   VIOLATION / { if (match($0, /:[0-9]+:/)) print substr($0, RSTART + 1, RLENGTH - 2) }' |
      sort -n >"$reported_sites"
    {
      printf '## the throw scan against its own control (exit %s, 1 is expected: nothing is allowed there)\n' \
        "$control_rc"
      printf '# control: %s\n' "${control#"$ROOT"/}"
      printf '%s\n' "$control_out"
      printf '# marked sites: %s\n' "$(tr '\n' ' ' <"$expected_sites")"
      printf '# reported sites: %s\n\n' "$(tr '\n' ' ' <"$reported_sites")"
    } >>"$EV"
    if [[ ! -s "$expected_sites" ]]; then
      detail "the throw-scan control carries no marked site, so it proves nothing"
      failed=1
    elif ! diff -u "$expected_sites" "$reported_sites" >>"$EV" 2>&1; then
      detail "the lexical throw scan does not report exactly the marked sites of its own control"
      failed=1
    fi

    # And the walk's own fail-closed control: a root that is not there must
    # be reported as a gap (status 2), never as a tree with no throw in it.
    local missing_root="$control_dir/absent-root"
    local missing_out=""
    local missing_rc=0
    missing_out="$(rule5_throw_scan - "$missing_root" 2>&1)" || missing_rc=$?
    {
      printf '## the throw scan against an absent root (2 is the pass: a gap, not an absence of throws)\n'
      printf '# root: %s\n' "${missing_root#"$ROOT"/}"
      printf '%s\n# exit: %s\n\n' "$missing_out" "$missing_rc"
    } >>"$EV"
    if [[ -e "$missing_root" ]]; then
      detail "the throw-scan walk control needs a path that does not exist, and $missing_root does"
      failed=1
    elif [[ "$missing_rc" -ne 2 ]]; then
      detail "the lexical throw scan read an unreachable root as an absence of throws (exit $missing_rc, expected 2)"
      failed=1
    fi
  fi

  run_sbt gate05-failable "testOnly *SmartConstructorSpec *FailableSurfaceSpec *ApiSurfaceSpec *FailureSpec" || rc=$?
  {
    command_line 'sbt -batch "testOnly *SmartConstructorSpec *FailableSurfaceSpec *ApiSurfaceSpec *FailureSpec"'
    printf '# sbt exit status: %s\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    awk '/Total number of tests run|Tests: succeeded|All tests passed/ { print }' "$SBT_LOG"
  } >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "the failable-surface specs exited $rc"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "no null, no throw expression of any spelling outside ArgCheck.scala (scan verified against its own control), failable-surface specs green"
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
    command_line "grep -rlE \"cats\\.effect|\\bIO\\[\" $BASICS_MAIN | grep -v /demo/"
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
    command_line "grep -rlE \"cats\\.effect|\\bIO\\[\" $COLLECT_MAIN | grep -v /io/Resources.scala"
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
    command_line 'sbt -batch "testOnly *TypeclassLawsSpec"'
    printf '# sbt exit status: %s\n' "$rc"
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
    command_line 'sbt -batch "testOnly *NamedEnumClosedSpec *ReferenceDataManifestSpec"'
    printf '# sbt exit status: %s\n' "$rc"
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
    command_line 'grep -rn "\.ini\|\.csv\|\.properties" strata-collect/src/main strata-basics/src/main'
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
# Row 11a - Gate 5 / Rule 4 and the construction policy of AAP section 0.3.3:
# construction and Java serialization are closed on the JVM, and not only in
# the Scala source that seals them.
#
#   Rule 4's closed families, and the representation every validated and
#   normalising type uses - `sealed abstract case class X private (...)`,
#   which generates neither `apply` nor `copy` - are enforced by scalac and by
#   nothing else. In the class file:
#
#     * `sealed` leaves no trace: this language version emits no
#       `PermittedSubclasses` attribute, so a family's base class and a
#       validated type are ordinary extensible public abstract classes;
#     * a `private` or `private[pkg]` constructor is emitted PUBLIC, the JVM
#       having no matching access level;
#     * the compiler gives every case class and case object a
#       `java.io.Serializable` supertype, so `java.io.ObjectInputStream` has a
#       second construction path into each of them, which fills fields no
#       factory validated.
#
#   A caller compiled against these class files by another language could
#   therefore mint the dynamic currencies, indices and conventions this port
#   deliberately removed, and forge validated values holding exactly the
#   inputs their factories reject. Both routes are closed in the sources: the
#   base class of every closed type runs a construction guard in its own
#   constructor - `JvmClosure.requireSoleImplementation` for a validated or
#   normalising type, `JvmClosure.requireDeclaredMember` for a named family -
#   which admits only the implementations the type itself declares, and every
#   product of both modules mixes in `NoJavaSerialization`, whose
#   `writeReplace` and `readResolve` refuse the write and the read path.
#
#   Two facts decide the shape of this row, and both were measured rather than
#   assumed. First, `private final class Impl` is emitted as an ACC_PUBLIC
#   class with an ACC_PUBLIC constructor: only the `InnerClasses` attribute
#   records the `private`, which `javac` honours and a class file emitted
#   without a compiler does not, so IDENTITY OF THE CLASS BEING CONSTRUCTED IS
#   NOT CLOSURE ON ITS OWN - what closes it is each type restating, over the
#   fields it holds, the invariant its factory establishes. Second, a `sealed
#   trait` compiles to a plain JVM interface with no closure at all, so the
#   roots of the closed hierarchies - `Index`, `FloatingRateIndex` and
#   `RateIndex` - are abstract classes whose constructors refuse a
#   subtype outside the families they admit. `IndexObservation` is not among
#   them: it is an open contract by design (divergence row 46 of
#   SCALA_MIGRATION.md), so no closure is claimed for it.
#
#   This row measures the closure in eight parts, five of which are attacks:
#
#     1. a reflective sweep over every compiled class of both modules: every
#        `scala.Product` refuses Java serialization; every concrete subclass
#        of an abstract class of these modules is declared inside that class
#        or one of its ancestors and is final, abstract or a singleton; and
#        nothing else taking part in Java serialization holds data of the
#        library (the residue is the compiler's own encoding - singleton
#        modules, derivation classes, lambdas - counted and reported);
#     2. the same program DERIVES the closed types, a closed type being an
#        abstract class with an implementation declared inside it, and
#        generates the attack sources of parts 4 and 5 from them;
#     3. a bytecode sweep derived from the SOURCES instead: every
#        `sealed abstract case class _ private` and every
#        `sealed abstract class _ private[pkg]` calls its guard in its own
#        constructor, and the total agrees with the count part 2 derived -
#        so a type that lost its guard, or an implementation that moved out
#        of its family, fails here whichever side it is seen from;
#     4. attack one, compiled by javac: naming a hidden implementation class
#        must FAIL to compile, for every hidden implementation both modules
#        hold;
#     5. attack two, compiled by javac and then run: an external subclass of
#        every closed type must compile - the JVM does permit it - and must
#        not construct. Each attempt is classified as refused by the guard,
#        refused before the guard was reached (a base class deriving a field
#        from a constructor argument raises on the synthetic argument while
#        evaluating the `super` call), or constructed, the last being the
#        finding this row exists to report;
#     6. attack three, compiled by javac and then run: a foreign subtype of
#        every level of every closed hierarchy, derived in part 2 as an
#        abstract class of these modules that another abstract class of theirs
#        extends. Each must compile and none may construct, which is what
#        makes a match the Scala compiler proved exhaustive exhaustive at run
#        time as well;
#     7. attack four, the emitted-bytecode route part 4 does not cover: a stub
#        tree declares each hidden implementation as a top-level class under
#        its BINARY name (`$` being a legal Java identifier character), an
#        attacker is compiled against that stub so that its bytecode carries a
#        plain `new` and `invokespecial` on the real binary constructor, and it
#        is then run against the real classes. The instructions are shown in
#        the evidence, and every forged state must be refused BY THE
#        INVARIANT, identified in the refusal message, rather than by chance;
#     8. attack five, a real forged object stream: a stub declares the type
#        with OVERRIDABLE serialization hooks, a subclass overriding both to
#        return itself is compiled against it, a stream carrying that subclass
#        is written with the stub on the classpath, and the stream is read
#        against the real classes - where `ObjectInputStream.readObject` must
#        itself fail, the hooks being `final` and the JVM rejecting a class
#        that overrides a final method when it is loaded.
#
#   Everything this row generates lands under `target/audit/jvm-closure/`,
#   which is git-ignored and outside every tree the other rows scan, so
#   Gate 2a's "no `.java` under either module or the build definition" scan
#   and the repository-boundary row are untouched by it.
#=============================================================================

# closure_metric <line prefix> <file> - the number a line of the audit's
# output ends with, or the empty string when the line is absent. The prefix is
# matched at the start of the line so that two metrics sharing a word cannot
# be confused.
closure_metric() {
  awk -v key="$1" 'index($0, key) == 1 {
    value = $0
    sub(/^[^=]*=[[:space:]]*/, "", value)
    print value
    exit
  }' "$2"
}

row_11a_jvm_closure() {
  new_evidence "jvm-closure.txt" || return 1
  local failed=0
  local rc=0
  local work="$AUDIT_DIR/jvm-closure"

  {
    printf '## Gate 5 / Rule 4 - JVM construction and serialization closure\n'
    command_line 'sbt -batch "export strata-basics/Compile/fullClasspath"'
    command_line 'javap -p over every class of both modules, then javac and java over the generated probes: Java serialization of each product, construction of each closed type, an external subclass of each sealed hierarchy, and the binary constructor of each hidden implementation'
    printf '\n'
  } >>"$EV"

  # A fresh tree per run: a probe or a forged subclass left by an earlier run
  # would be compiled and counted by this one.
  if ! rm -rf "$work"; then
    detail "the working directory ${work#"$ROOT"/} could not be cleared"
    return 1
  fi
  if ! mkdir -p "$work/src" "$work/emit"; then
    detail "the working directory ${work#"$ROOT"/} could not be created"
    return 1
  fi

  # -- 1. the compiled output and a runtime classpath ----------------------
  local directory
  for directory in "$COLLECT_CLASSES" "$BASICS_CLASSES"; do
    if [[ ! -d "$ROOT/$directory" ]]; then
      detail "$directory is missing: the modules must be compiled before this row"
      return 1
    fi
  done

  rc=0
  run_sbt gate11a-classpath "export strata-basics/Compile/fullClasspath" || rc=$?
  local classpath_log="$SBT_LOG"
  # `export` prints the value unprefixed; sbt's own chatter is `[...]`-prefixed.
  # The root project aggregates the other, so `export` prints BOTH projects'
  # values - and strata-collect's classpath does not carry the basics classes.
  # The line is therefore selected by carrying both modules' output AND
  # `scala-library`: a line missing any of the three is not the classpath this
  # row needs, and compiling the attacks against it would fail for the wrong
  # reason - which is how a javac that refused every probe because a package
  # was absent could be read as a javac that refused it for its access.
  local classpath
  classpath="$(awk -v collect="$COLLECT_CLASSES" -v basics="$BASICS_CLASSES" \
    '!/^\[/ && /scala-library/ && index($0, collect) && index($0, basics) { print; exit }' \
    "$classpath_log")"
  {
    printf '## Gate 5 / Rule 4 - JVM construction and serialization closure\n'
    printf '# classpath log: %s (sbt exit %s)\n' "${classpath_log#"$ROOT"/}" "$rc"
    printf '# classpath entries: %s\n' "$(printf '%s' "$classpath" | tr ':' '\n' | awk 'NF { n++ } END { print n + 0 }')"
    printf '# generated attack sources: %s\n\n' "${work#"$ROOT"/}"
  } >>"$EV"
  if [[ "$rc" -ne 0 || -z "$classpath" ]]; then
    detail "the Compile classpath of strata-basics could not be exported (sbt exit $rc)"
    return 1
  fi

  # -- 2. the reflective sweep, which also generates the attacks -----------
  local audit_source="$work/src/JvmClosureAudit.java"
  if ! cat >"$audit_source" <<'JAVA'
package audit;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.stream.Stream;

/**
 * Audits the JVM-level construction and serialization closure of the two Scala modules, and
 * generates the Java sources that attack it.
 *
 * Usage: JvmClosureAudit <emit-dir> <classes-dir>...
 *
 * The reflective half asserts three properties over every class the modules emit:
 *   1. every class carrying the compiler's product encoding refuses Java serialization, by being
 *      a subtype of com.opengamma.strata.collect.NoJavaSerialization;
 *   2. every concrete subclass of an abstract class of these modules is declared inside that
 *      class or one of its ancestors, and is final, abstract or a singleton module - so no
 *      anonymous or foreign implementation is published;
 *   3. nothing else that takes part in Java serialization holds data of the library: the residue
 *      is the compiler's own encoding (singleton modules, derivation classes, lambdas).
 *
 * The generating half writes, into the directory named first:
 *   probe/   one Java source per hidden implementation class, naming it. Every one of them MUST
 *            fail to compile: that is the gate's proof that the implementations are unreachable.
 *   forge/   one Java source per closed type that Java could extend, declaring a subclass of it
 *            with a no-argument constructor, plus a runner. Every one of them MUST compile - the
 *            JVM does permit the subclass - and MUST raise at construction, which is the gate's
 *            proof that the guard closes what the class file leaves open.
 */
public final class JvmClosureAudit {

  /** The package prefix of the two modules being audited. */
  private static final String MODULE_PACKAGE = "com.opengamma.strata.";

  public static void main(String[] args) throws Exception {
    if (args.length < 2) {
      System.err.println("usage: JvmClosureAudit <emit-dir> <classes-dir>...");
      System.exit(2);
    }
    Path emitDirectory = Path.of(args[0]);
    List<Path> roots = new ArrayList<>();
    List<URL> urls = new ArrayList<>();
    for (int index = 1; index < args.length; index++) {
      Path root = Path.of(args[index]);
      roots.add(root);
      urls.add(root.toUri().toURL());
    }
    for (String entry : System.getProperty("java.class.path").split(File.pathSeparator)) {
      urls.add(Path.of(entry).toUri().toURL());
    }
    ClassLoader loader = new URLClassLoader(urls.toArray(new URL[0]), JvmClosureAudit.class.getClassLoader());
    Class<?> product = loader.loadClass("scala.Product");
    Class<?> refusal = loader.loadClass("com.opengamma.strata.collect.NoJavaSerialization");

    List<Class<?>> classes = load(roots, loader);
    boolean passed = true;
    passed &= reportSerializationClosure(classes, product, refusal);
    java.util.Set<Class<?>> closedTypes = new java.util.LinkedHashSet<>();
    passed &= reportImplementationClosure(classes, closedTypes);
    passed &= reportBinaryEntryPoints(classes);
    passed &= reportForgeableTypes(classes, closedTypes);
    passed &= emitProbes(classes, emitDirectory);
    passed &= emitForges(closedTypes, emitDirectory);
    passed &= emitRootForges(deriveRoots(classes, closedTypes), emitDirectory);
    System.out.println("verdict = " + (passed ? "PASS" : "FAIL"));
    System.exit(passed ? 0 : 1);
  }

  /** Loads every class file under the given roots, without initialising any of them. */
  private static List<Class<?>> load(List<Path> roots, ClassLoader loader) throws IOException {
    List<Class<?>> classes = new ArrayList<>();
    for (Path root : roots) {
      try (Stream<Path> files = Files.walk(root)) {
        for (Path file : files.filter(path -> path.toString().endsWith(".class")).toList()) {
          String name = root.relativize(file).toString().replace(File.separatorChar, '.');
          name = name.substring(0, name.length() - ".class".length());
          try {
            classes.add(Class.forName(name, false, loader));
          } catch (Throwable failure) {
            System.out.println("UNLOADABLE " + name + ": " + failure);
          }
        }
      }
    }
    classes.sort(Comparator.comparing(Class::getName));
    System.out.println("classes audited                       = " + classes.size());
    return classes;
  }

  /** Property 1 and property 3: the two halves of the serialization closure. */
  private static boolean reportSerializationClosure(
      List<Class<?>> classes, Class<?> product, Class<?> refusal) throws ClassNotFoundException {
    Class<?> serializable = Class.forName("java.io.Serializable");
    List<String> unrefused = new ArrayList<>();
    List<String> residue = new ArrayList<>();
    int products = 0;
    int modules = 0;
    int generated = 0;
    for (Class<?> candidate : classes) {
      boolean refuses = refusal.isAssignableFrom(candidate);
      if (product.isAssignableFrom(candidate)) {
        products++;
        if (!refuses) { unrefused.add(candidate.getName()); }
        continue;
      }
      if (!serializable.isAssignableFrom(candidate) || refuses) { continue; }
      String name = candidate.getName();
      if (name.endsWith("$")) { modules++; }
      else if (name.contains("$anon") || name.contains("$$Lambda")) { generated++; }
      else { residue.add(name); }
    }
    System.out.println("product classes                       = " + products);
    System.out.println("products not refusing serialization    = " + unrefused.size());
    unrefused.forEach(name -> System.out.println("   UNREFUSED " + name));
    System.out.println("serializable residue: modules = " + modules
        + ", derivation/lambda = " + generated + ", other = " + residue.size());
    residue.forEach(name -> System.out.println("   OTHER-SERIALIZABLE " + name));
    return unrefused.isEmpty() && residue.isEmpty() && products > 150;
  }

  /** Property 2: no implementation of an abstract type of these modules is published. */
  private static boolean reportImplementationClosure(
      List<Class<?>> classes, java.util.Set<Class<?>> closedTypes) {
    Map<String, TreeSet<String>> open = new LinkedHashMap<>();
    int checked = 0;
    for (Class<?> candidate : classes) {
      Class<?> parent = candidate.getSuperclass();
      if (parent == null || !parent.getName().startsWith(MODULE_PACKAGE)) { continue; }
      if (!Modifier.isAbstract(parent.getModifiers())) { continue; }
      checked++;
      // A type with an implementation declared inside it is a closed type of this port: a
      // validated or normalising type, or the head of a named family. That is derived here
      // rather than listed, so the forge below attacks the closed types the modules actually
      // publish - and an extension point, which has no implementation of its own inside it,
      // is not mistaken for one.
      for (Class<?> ancestor = parent; ancestor != null; ancestor = ancestor.getSuperclass()) {
        if (ancestor.getName().startsWith(MODULE_PACKAGE) && Modifier.isAbstract(ancestor.getModifiers())
            && candidate.getDeclaringClass() == ancestor) {
          closedTypes.add(parent);
        }
      }
      // A subclass is closed in one of three ways the class file can express, and this reports
      // the ones that are closed in none of them - a class something outside these modules could
      // extend, and the anonymous `new X(...) {}` form the validated types were first written
      // with, which the compiler publishes with a public constructor and which no declaration
      // names. An abstract candidate passes without being nested inside its parent, because no
      // instance of it exists and the concrete classes extending it are checked by this same
      // loop; that is also the only shape a multi-level closed hierarchy can take, and those
      // levels are attacked in their own right by the root forge below. A named final class
      // passes likewise: a total type is published as itself.
      boolean named = !candidate.isAnonymousClass() && !candidate.isSynthetic()
          && !candidate.getName().contains("$anon$");
      boolean abstractLevel = Modifier.isAbstract(candidate.getModifiers());
      boolean closedShape = abstractLevel
          || candidate.getName().endsWith("$")
          || (named && Modifier.isFinal(candidate.getModifiers()));
      if (!closedShape) {
        open.computeIfAbsent(parent.getName(), key -> new TreeSet<>())
            .add(candidate.getName()
                + (named ? "" : " [no declaration names this class]")
                + (Modifier.isFinal(candidate.getModifiers()) ? "" : " [neither final nor a singleton]"));
      }
    }
    System.out.println("concrete subclasses of abstract types  = " + checked);
    System.out.println("closed types (implementation inside)   = " + closedTypes.size());
    System.out.println("implementations not declared in family = " + open.size());
    open.forEach((parent, kids) -> kids.forEach(kid -> System.out.println("   OPEN " + parent + " <- " + kid)));
    return open.isEmpty() && checked > 100;
  }

  /**
   * Reports which closed types have an implementation whose constructor CARRIES STATE.
   *
   * The distinction decides what a closed type has to check. An implementation whose constructor
   * takes no argument - the `case object` members of a convention family are the case - holds no
   * field a forged construction could set, so identity is the whole of its closure and there is
   * no invariant to state. An implementation that takes arguments can be handed any values that
   * type-check, through the public constructor part 7 attacks, so its type must restate the
   * invariant its factory establishes. The two sets are derived here and the source-derived sweep
   * of part 3 requires an invariant of every member of the first, so a type that gains a
   * stateful implementation later gains the obligation with it.
   */
  private static boolean reportForgeableTypes(
      List<Class<?>> classes, java.util.Set<Class<?>> closedTypes) {
    java.util.Set<String> stateful = new TreeSet<>();
    java.util.Set<String> stateless = new TreeSet<>();
    for (Class<?> closed : closedTypes) {
      boolean carriesState = false;
      for (Class<?> candidate : classes) {
        if (candidate == closed) { continue; }
        if (!closed.isAssignableFrom(candidate)) { continue; }
        if (Modifier.isAbstract(candidate.getModifiers())) { continue; }
        for (Constructor<?> constructor : candidate.getDeclaredConstructors()) {
          if (constructor.getParameterCount() > 0) { carriesState = true; }
        }
      }
      if (carriesState) { stateful.add(closed.getName() + " " + chainOf(classes, closed)); }
      else { stateless.add(closed.getName()); }
    }
    System.out.println("closed types with stateful implementations = " + stateful.size());
    System.out.println("closed types with stateless implementations = " + stateless.size());
    stateful.forEach(name -> System.out.println("   FORGEABLE " + name));
    stateless.forEach(name -> System.out.println("   STATELESS " + name));
    return stateful.size() + stateless.size() == closedTypes.size() && !stateful.isEmpty();
  }

  /**
   * The classes on the construction path of every stateful implementation of a closed type.
   *
   * The invariant of a value belongs where its fields are declared, which is not always the head
   * of the family: `DayCount` publishes twenty-one `case object` members holding nothing and one
   * `Bus252` holding a calendar, so the condition that its name matches that calendar is stated
   * by `Bus252` and not by `DayCount`; `RollConvention` is the same, with the day-of-month and
   * day-of-week members holding the only fields. The chain named here is therefore every class
   * from each stateful implementation up to and including the closed type, and the source-derived
   * half requires the invariant to appear in the constructor of one of them.
   */
  private static String chainOf(List<Class<?>> classes, Class<?> closed) {
    java.util.Set<String> chain = new java.util.LinkedHashSet<>();
    for (Class<?> candidate : classes) {
      if (Modifier.isAbstract(candidate.getModifiers()) || !closed.isAssignableFrom(candidate)) { continue; }
      boolean carriesState = false;
      for (Constructor<?> constructor : candidate.getDeclaredConstructors()) {
        if (constructor.getParameterCount() > 0) { carriesState = true; }
      }
      if (!carriesState) { continue; }
      for (Class<?> step = candidate; step != null; step = step.getSuperclass()) {
        chain.add(step.getName());
        if (step == closed) { break; }
      }
    }
    return String.join(";", chain);
  }

  /**
   * Reports how many hidden implementation classes expose a PUBLIC constructor in the class file.
   *
   * This is the hole the invariants exist to close, measured rather than assumed. A hidden
   * implementation is `private final class Impl` in the Scala source, and `javac` honours that
   * through the `InnerClasses` attribute - which is what the probes of part 4 prove. The class
   * and its constructor are nevertheless both `ACC_PUBLIC` in the class file, because this
   * language version emits no nest members and the JVM has no matching access level, so a class
   * file produced without a Java or Scala compiler reaches the constructor directly. The count
   * is reported so that a reader can see the size of the surface the invariant checks cover, and
   * it must be non-trivial or the attacks of part 7 would be testing an empty set.
   */
  private static boolean reportBinaryEntryPoints(List<Class<?>> classes) {
    int reachable = 0;
    int restricted = 0;
    for (Class<?> candidate : classes) {
      if (!candidate.getName().startsWith(MODULE_PACKAGE)) { continue; }
      if (!Modifier.isPrivate(candidate.getModifiers())) { continue; }
      if (candidate.getDeclaringClass() == null) { continue; }
      boolean open = false;
      for (Constructor<?> constructor : candidate.getDeclaredConstructors()) {
        if (Modifier.isPublic(constructor.getModifiers())) { open = true; }
      }
      if (open) { reachable++; } else { restricted++; }
    }
    System.out.println("binary entry points reachable         = " + reachable);
    System.out.println("binary entry points restricted        = " + restricted);
    return reachable >= 30;
  }

  /**
   * Derives the roots and intermediates of the closed hierarchies of these modules.
   *
   * A root carries no data and declares no implementation of its own, so the derivation of part 2
   * - an abstract class with an implementation declared inside it - cannot see it, and it is
   * exactly the shape a foreign class file can claim most cheaply. The rule here is the
   * complementary one: an abstract class of these modules that another ABSTRACT class of theirs
   * extends is a level of a hierarchy rather than a leaf, and every level must refuse a subtype
   * that belongs to none of the families it admits. `Index`, `FloatingRateIndex` and `RateIndex`
   * are the cases, and they are found rather than named, so a hierarchy added
   * later is attacked by this row without it being edited.
   */
  private static java.util.Set<Class<?>> deriveRoots(
      List<Class<?>> classes, java.util.Set<Class<?>> closedTypes) {
    java.util.Set<Class<?>> roots = new java.util.LinkedHashSet<>();
    for (Class<?> candidate : classes) {
      if (!candidate.getName().startsWith(MODULE_PACKAGE)) { continue; }
      if (!Modifier.isAbstract(candidate.getModifiers()) || candidate.isInterface()) { continue; }
      Class<?> parent = candidate.getSuperclass();
      if (parent == null || !parent.getName().startsWith(MODULE_PACKAGE)) { continue; }
      if (!Modifier.isAbstract(parent.getModifiers()) || parent.isInterface()) { continue; }
      if (closedTypes.contains(parent)) { continue; }
      roots.add(parent);
    }
    System.out.println("closed hierarchy levels derived       = " + roots.size());
    // Each level is printed with the levels above it, because the subtype guard is run by the
    // ROOT of a hierarchy and inherited by every level beneath it: `RateIndex` declares no guard
    // of its own, and does not need one, since its constructor calls `FloatingRateIndex`'s which
    // calls `Index`'s, which is where the guard is.
    roots.forEach(root -> {
      StringBuilder chain = new StringBuilder();
      for (Class<?> step = root; step != null && step.getName().startsWith(MODULE_PACKAGE);
          step = step.getSuperclass()) {
        if (chain.length() > 0) { chain.append(';'); }
        chain.append(step.getName());
      }
      System.out.println("   ROOT " + root.getName() + " " + chain);
    });
    return roots;
  }

  /**
   * Writes one Java subclass per derived hierarchy level, plus a runner that constructs each.
   *
   * Each must COMPILE - the level is a public abstract class whose constructor the class file
   * publishes, so the JVM permits the subclass - and none may construct. What refuses them is
   * `JvmClosure.requirePermittedSubtype` in the level's own constructor, which is the check that
   * makes a match the Scala compiler proved exhaustive exhaustive at run time as well.
   */
  private static boolean emitRootForges(java.util.Set<Class<?>> roots, Path emitDirectory)
      throws IOException {
    Path directory = emitDirectory.resolve("rootforge");
    Files.createDirectories(directory);
    List<String> forged = new ArrayList<>();
    List<String> skipped = new ArrayList<>();
    for (Class<?> candidate : roots) {
      if (!Modifier.isPublic(candidate.getModifiers()) || candidate.getCanonicalName() == null) {
        skipped.add(candidate.getName() + " [not nameable from Java]");
        continue;
      }
      Constructor<?> constructor = narrowest(candidate);
      if (constructor == null) { skipped.add(candidate.getName() + " [no reachable constructor]"); continue; }
      String arguments = argumentsFor(constructor);
      if (arguments == null) { skipped.add(candidate.getName() + " [constructor names a hidden type]"); continue; }
      String stubs = stubsFor(candidate);
      if (stubs == null) { skipped.add(candidate.getName() + " [abstract member names a hidden type]"); continue; }
      String name = "RootForge_" + (forged.size() + 1);
      Files.writeString(directory.resolve(name + ".java"),
          "package audit.rootforge;\n\n"
              + "/** A foreign subtype of " + candidate.getName() + ", which must not be constructible. */\n"
              + "public final class " + name + " extends " + candidate.getCanonicalName() + " {\n"
              + "  public " + name + "() {\n"
              + "    super(" + arguments + ");\n"
              + "  }\n"
              + stubs
              + "}\n");
      forged.add(name + " extends " + candidate.getName());
    }
    StringBuilder runner = new StringBuilder();
    runner.append("package audit.rootforge;\n\n")
        .append("/** Constructs a foreign subtype of every closed hierarchy level. */\n")
        .append("public final class RootForgeRunner {\n")
        .append("  public static void main(String[] args) {\n")
        .append("    int guarded = 0;\n")
        .append("    int otherwise = 0;\n")
        .append("    int admitted = 0;\n");
    for (int index = 0; index < forged.size(); index++) {
      String name = "RootForge_" + (index + 1);
      String label = forged.get(index).replace("\"", "");
      runner.append("    try { new ").append(name).append("(); admitted++; System.out.println(\"ADMITTED ")
          .append(label).append("\"); }\n")
          .append("    catch (IllegalArgumentException refusal) { guarded++; System.out.println(\"REFUSED-BY-GUARD ")
          .append(label).append(" -> \" + refusal.getMessage()); }\n")
          .append("    catch (Throwable other) { otherwise++; System.out.println(\"REFUSED-OTHERWISE ")
          .append(label).append(" -> \" + other); }\n");
    }
    runner.append("    System.out.println(\"foreign subtypes of a hierarchy level = \" + (guarded + otherwise + admitted));\n")
        .append("    System.out.println(\"hierarchy levels refusing a foreign subtype = \" + guarded);\n")
        .append("    System.out.println(\"hierarchy levels refusing before the guard = \" + otherwise);\n")
        .append("    System.out.println(\"hierarchy levels admitting a foreign subtype = \" + admitted);\n")
        .append("    System.exit(admitted == 0 && guarded >= 3 ? 0 : 1);\n")
        .append("  }\n}\n");
    Files.writeString(directory.resolve("RootForgeRunner.java"), runner.toString());
    System.out.println("foreign hierarchy subtypes generated  = " + forged.size());
    System.out.println("hierarchy levels not forgeable        = " + skipped.size());
    skipped.forEach(entry -> System.out.println("   NOT-FORGED-ROOT " + entry));
    return forged.size() >= 3;
  }

  /** Whether a class is declared inside its parent or inside one of the parent's ancestors. */
  private static boolean declaredInFamily(Class<?> candidate, Class<?> parent) {
    Class<?> declaring = candidate.getDeclaringClass();
    if (declaring == null) { return false; }
    for (Class<?> ancestor = parent; ancestor != null; ancestor = ancestor.getSuperclass()) {
      if (declaring == ancestor) { return true; }
    }
    return false;
  }

  /**
   * Writes one Java source per hidden implementation class, naming that class and nothing else.
   * Every one must fail to compile.
   */
  private static boolean emitProbes(List<Class<?>> classes, Path emitDirectory) throws IOException {
    Path directory = emitDirectory.resolve("probe");
    Files.createDirectories(directory);
    int written = 0;
    for (Class<?> candidate : classes) {
      if (!candidate.getName().startsWith(MODULE_PACKAGE)) { continue; }
      if (!Modifier.isPrivate(candidate.getModifiers())) { continue; }
      if (candidate.getDeclaringClass() == null || candidate.getCanonicalName() == null) { continue; }
      written++;
      String name = "Probe_" + written;
      Files.writeString(directory.resolve(name + ".java"),
          "package audit.probe;\n\n"
              + "/** Names " + candidate.getName() + ", which must not be nameable. */\n"
              + "final class " + name + " {\n"
              + "  static final Class<?> HIDDEN = " + candidate.getCanonicalName() + ".class;\n"
              + "}\n");
    }
    System.out.println("hidden implementation probes written   = " + written);
    return written >= 30;
  }

  /**
   * Writes one Java subclass per closed type Java is able to extend, plus a runner that tries to
   * construct each of them. Every one must compile and every one must raise.
   */
  private static boolean emitForges(java.util.Set<Class<?>> closedTypes, Path emitDirectory)
      throws IOException {
    Path directory = emitDirectory.resolve("forge");
    Files.createDirectories(directory);
    List<String> forged = new ArrayList<>();
    List<String> skipped = new ArrayList<>();
    for (Class<?> candidate : closedTypes) {
      if (!Modifier.isPublic(candidate.getModifiers()) || candidate.getCanonicalName() == null) {
        skipped.add(candidate.getName() + " [not nameable from Java]");
        continue;
      }
      Constructor<?> constructor = narrowest(candidate);
      if (constructor == null) { skipped.add(candidate.getName() + " [no reachable constructor]"); continue; }
      String arguments = argumentsFor(constructor);
      if (arguments == null) { skipped.add(candidate.getName() + " [constructor names a hidden type]"); continue; }
      String stubs = stubsFor(candidate);
      if (stubs == null) { skipped.add(candidate.getName() + " [abstract member names a hidden type]"); continue; }
      String name = "Forge_" + (forged.size() + 1);
      Files.writeString(directory.resolve(name + ".java"),
          "package audit.forge;\n\n"
              + "/** An external subclass of " + candidate.getName() + ", which must not be constructible. */\n"
              + "public final class " + name + " extends " + candidate.getCanonicalName() + " {\n"
              + "  public " + name + "() {\n"
              + "    super(" + arguments + ");\n"
              + "  }\n"
              + stubs
              + "}\n");
      forged.add(name + " extends " + candidate.getName());
    }
    // The runner classifies each attempt three ways, because the two failures are not the same
    // claim. A refusal by the guard is the property this gate exists to assert. A refusal by
    // anything else still means no instance was created - a closed type whose base class derives
    // a field from a constructor argument raises on the synthetic argument while evaluating the
    // `super` call, before the guard is reached - and is reported separately rather than counted
    // as a guard refusal it is not. A construction that completes is the finding.
    StringBuilder runner = new StringBuilder();
    runner.append("package audit.forge;\n\n")
        .append("/** Constructs every forged subclass, and reports one line per attempt. */\n")
        .append("public final class ForgeRunner {\n")
        .append("  public static void main(String[] args) {\n")
        .append("    int guarded = 0;\n")
        .append("    int otherwise = 0;\n")
        .append("    int admitted = 0;\n");
    for (int index = 0; index < forged.size(); index++) {
      String name = "Forge_" + (index + 1);
      String label = forged.get(index).replace("\"", "");
      runner.append("    try { new ").append(name).append("(); admitted++; System.out.println(\"ADMITTED ")
          .append(label).append("\"); }\n")
          .append("    catch (IllegalArgumentException refusal) { guarded++; System.out.println(\"REFUSED-BY-GUARD ")
          .append(label).append("\"); }\n")
          .append("    catch (Throwable other) { otherwise++; System.out.println(\"REFUSED-OTHERWISE ")
          .append(label).append(" -> \" + other); }\n");
    }
    runner.append("    System.out.println(\"forged subclasses = \" + (guarded + otherwise + admitted));\n")
        .append("    System.out.println(\"refused by the construction guard = \" + guarded);\n")
        .append("    System.out.println(\"refused before the guard was reached = \" + otherwise);\n")
        .append("    System.out.println(\"constructed successfully = \" + admitted);\n")
        .append("    System.exit(admitted == 0 && guarded >= 40 ? 0 : 1);\n")
        .append("  }\n}\n");
    Files.writeString(directory.resolve("ForgeRunner.java"), runner.toString());
    System.out.println("external subclasses generated          = " + forged.size());
    System.out.println("closed types not forgeable from Java   = " + skipped.size());
    skipped.forEach(entry -> System.out.println("   NOT-FORGED " + entry));
    return forged.size() >= 40;
  }

  /**
   * The member implementations a Java subclass of this class has to supply, as source.
   *
   * A closed type may leave members abstract - a name a data table supplies, a calculation each
   * member defines - and Java will not compile a subclass that does not implement them. They are
   * stubbed here so that the attack the forge represents is compiled and run rather than declared
   * impossible: the stubs are never called, because construction raises before the object exists.
   *
   * @param candidate  the class being subclassed
   * @return the stub declarations, or null when a member's signature names a type Java cannot see
   */
  private static String stubsFor(Class<?> candidate) {
    StringBuilder stubs = new StringBuilder();
    // Every method reachable on the candidate, grouped by the signature a Java override has to
    // match - the name and the parameter types, the return type being free to narrow. Grouping is
    // what keeps a multi-level hierarchy stubbable: a level may declare a member abstract and
    // return a general type where the level below narrows it, and a level may declare it abstract
    // where a leaf implements it FINAL. Emitting the declaration as each level words it produces
    // a class that does not compile - "return type Index is not compatible with FxIndex", or
    // "overridden method is final" - so the group decides once, for all of them.
    Map<String, List<Method>> bySignature = new LinkedHashMap<>();
    List<Method> reachable = new ArrayList<>();
    for (Method method : candidate.getMethods()) { reachable.add(method); }
    for (Class<?> current = candidate; current != null; current = current.getSuperclass()) {
      for (Method method : current.getDeclaredMethods()) {
        if (!Modifier.isPrivate(method.getModifiers())) { reachable.add(method); }
      }
    }
    for (Method method : reachable) {
      StringBuilder key = new StringBuilder(method.getName()).append('(');
      for (Class<?> parameter : method.getParameterTypes()) { key.append(parameter.getName()).append(','); }
      bySignature.computeIfAbsent(key.append(')').toString(), signature -> new ArrayList<>()).add(method);
    }
    List<Method> abstractMethods = new ArrayList<>();
    for (List<Method> group : bySignature.values()) {
      // A signature something on the chain implements needs no stub, and must not have one: the
      // implementation may be final, and even where it is not, overriding it is not the attack.
      boolean implemented = false;
      for (Method method : group) {
        if (!Modifier.isAbstract(method.getModifiers())) { implemented = true; }
      }
      if (implemented) { continue; }
      // Otherwise the most specific return type among the declarations, which is the only one a
      // single override can satisfy.
      Method narrowest = group.get(0);
      for (Method method : group) {
        if (narrowest.getReturnType().isAssignableFrom(method.getReturnType())) { narrowest = method; }
      }
      abstractMethods.add(narrowest);
    }
    for (Method method : abstractMethods) {
      StringBuilder parameters = new StringBuilder();
      boolean nameable = method.getReturnType().getCanonicalName() != null
          && !Modifier.isPrivate(method.getReturnType().getModifiers());
      int index = 0;
      for (Class<?> parameter : method.getParameterTypes()) {
        String type = parameter.getCanonicalName();
        if (type == null || Modifier.isPrivate(parameter.getModifiers())) { nameable = false; break; }
        if (index > 0) { parameters.append(", "); }
        parameters.append(type).append(" argument").append(index);
        index++;
      }
      if (!nameable) { return null; }
      String returnType = method.getReturnType().getCanonicalName();
      String body = method.getReturnType() == void.class
          ? ""
          : "return " + defaultValue(method.getReturnType()) + "; ";
      stubs.append("\n  @Override\n  public ").append(returnType).append(' ').append(method.getName())
          .append('(').append(parameters).append(") { ").append(body).append("}\n");
    }
    return stubs.toString();
  }

  /** The constructor with the fewest parameters that Java could call. */
  private static Constructor<?> narrowest(Class<?> candidate) {
    Constructor<?> chosen = null;
    for (Constructor<?> constructor : candidate.getDeclaredConstructors()) {
      if (!Modifier.isPublic(constructor.getModifiers()) && !Modifier.isProtected(constructor.getModifiers())) { continue; }
      if (chosen == null || constructor.getParameterCount() < chosen.getParameterCount()) { chosen = constructor; }
    }
    return chosen;
  }

  /** The argument list of a `super(...)` call that satisfies a constructor's signature. */
  private static String argumentsFor(Constructor<?> constructor) {
    StringBuilder arguments = new StringBuilder();
    for (Class<?> parameter : constructor.getParameterTypes()) {
      if (arguments.length() > 0) { arguments.append(", "); }
      String value = defaultValue(parameter);
      if (value == null) { return null; }
      arguments.append(value);
    }
    return arguments.toString();
  }

  /** A Java expression of the given type, typed so that no overload is ambiguous. */
  private static String defaultValue(Class<?> parameter) {
    if (parameter == boolean.class) { return "false"; }
    if (parameter == char.class) { return "(char) 0"; }
    if (parameter == byte.class) { return "(byte) 0"; }
    if (parameter == short.class) { return "(short) 0"; }
    if (parameter == int.class) { return "0"; }
    if (parameter == long.class) { return "0L"; }
    if (parameter == float.class) { return "0.0f"; }
    if (parameter == double.class) { return "0.0"; }
    String name = parameter.getCanonicalName();
    if (name == null || Modifier.isPrivate(parameter.getModifiers())) { return null; }
    return "(" + name + ") null";
  }
}
JAVA
  then
    detail "the audit program could not be written to ${audit_source#"$ROOT"/}"
    return 1
  fi

  rc=0
  javac -d "$work/classes" "$audit_source" >"$work/audit-compile.log" 2>&1 || rc=$?
  if [[ "$rc" -ne 0 ]]; then
    detail "the audit program did not compile (javac exit $rc)"
    cat "$work/audit-compile.log" >>"$EV"
    return 1
  fi

  local audit_output="$work/audit.txt"
  rc=0
  java -cp "$classpath:$work/classes" audit.JvmClosureAudit \
    "$work/emit" "$ROOT/$COLLECT_CLASSES" "$ROOT/$BASICS_CLASSES" >"$audit_output" 2>&1 || rc=$?
  cat "$audit_output" >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "the reflective closure sweep failed (exit $rc; see ${audit_output#"$ROOT"/})"
    failed=1
  fi

  local products unrefused open_implementations residue closed_types probes forges
  products="$(closure_metric 'product classes' "$audit_output")"
  unrefused="$(closure_metric 'products not refusing serialization' "$audit_output")"
  open_implementations="$(closure_metric 'implementations not declared in family' "$audit_output")"
  residue="$(closure_metric 'serializable residue' "$audit_output")"
  closed_types="$(closure_metric 'closed types (implementation inside)' "$audit_output")"
  probes="$(closure_metric 'hidden implementation probes written' "$audit_output")"
  forges="$(closure_metric 'external subclasses generated' "$audit_output")"
  local entry_points root_forges
  entry_points="$(closure_metric 'binary entry points reachable' "$audit_output")"
  root_forges="$(closure_metric 'foreign hierarchy subtypes generated' "$audit_output")"

  # Vacuity first: a sweep that found no products, no closed types and no
  # implementations to audit would report zero violations of everything.
  if [[ -z "$products" || "$products" -lt 150 ]]; then
    detail "the product sweep found ${products:-no} product classes, which is not the compiled output"
    failed=1
  fi
  if [[ -z "$closed_types" || "$closed_types" -lt 40 ]]; then
    detail "the sweep derived ${closed_types:-no} closed types, which is not this port's inventory"
    failed=1
  fi
  if [[ -z "$entry_points" || "$entry_points" -lt 30 ]]; then
    detail "the sweep found ${entry_points:-no} reachable binary entry points, so parts 3 and 7 would assert nothing"
    failed=1
  fi
  if [[ -z "$root_forges" || "$root_forges" -lt 3 ]]; then
    detail "the sweep derived ${root_forges:-no} closed hierarchy levels, which is not this port's inventory"
    failed=1
  fi
  if [[ "$unrefused" != "0" ]]; then
    detail "$unrefused compiled product(s) do not refuse Java serialization"
    failed=1
  fi
  if [[ "$open_implementations" != "0" ]]; then
    detail "$open_implementations abstract type(s) have an implementation declared outside them"
    failed=1
  fi
  if [[ "$residue" != *", other = 0"* ]]; then
    detail "a type of these modules takes part in Java serialization without refusing it: $residue"
    failed=1
  fi

  # -- 3. the same closure, derived from the sources and read in bytecode --
  local sweep="$work/guard-sweep.txt"
  rc=0
  python3 - "$COLLECT_MAIN" "$BASICS_MAIN" "$ROOT/$COLLECT_CLASSES" "$ROOT/$BASICS_CLASSES" \
    "${closed_types:-0}" "$audit_output" <<'PY' >"$sweep" 2>&1 || rc=$?
# The source-derived half of this row: the closed declarations are read out of
# the two module trees, and the guard of each is then read out of its own
# constructor in the compiled class. Deriving the inventory from the sources
# rather than from the reflective sweep is the point - the two are independent,
# and a type that lost its guard, or an implementation that moved out of its
# family, disagrees with the other side rather than silently shrinking both.
import re
import subprocess
import sys
from pathlib import Path

collect_main, basics_main, collect_classes, basics_classes, expected_closed, audit_output = sys.argv[1:7]
classpath = collect_classes + ":" + basics_classes

# The closed types whose implementations carry state, as the reflective half derived them. Each
# of these is reachable through a PUBLIC constructor in the class file - `private final class
# Impl` is honoured by javac through the `InnerClasses` attribute and by nothing in the JVM - so
# for these the identity of the class being constructed is not enough on its own: a class file
# emitted without a compiler can name the implementation and hand it any arguments that
# type-check. What closes that is the type restating, over the fields it holds, the invariant its
# factory establishes, and this requires one of every member of the set.
#
# Exempting one is a deliberate act, and it is recorded here with the reason, so that the set is
# read as a decision rather than as a gap. One type is in it.
#
# `CurrencyAmountArray` promises that every element of its values is a number, and that promise is
# established once, by whichever route takes the numbers in - `create` raises it, `checked` reports
# it, and the two factories that read `CurrencyAmount`s rest on the invariant that type states of
# every instance of itself. What it does not do is restate the promise in its class body, because
# this is the one invariant in either module that costs a pass over the whole run rather than a
# constant or a step per currency: a constructor runs for every value built, so restating it there
# examines every element of every array a second time, doubling the cost of `of` and of each
# element-wise operation on a run of a hundred thousand scenarios - for the one route the supported
# API does not have. The class body carries that reasoning at the point the restatement would go.
# `requireSoleImplementation` is still called, so a foreign subtype is still refused; what is no
# longer examined is an instance forged by a class file naming the private implementation directly,
# a route design decision D-3 does not support, and such a value is refused by `CurrencyAmount` as
# soon as any element of it is read as an amount.
#
# `MultiCurrencyAmountArray` is NOT exempt and is not listed: it keeps the two invariants that cost
# a constant and a step per currency - its size, and its per-currency array lengths, which are what
# stop a forged run being read past the end of one of its arrays - and loses only the per-element
# walk. Nothing else in either module is exempt.
TOTAL_BY_CONSTRUCTION = {"com.opengamma.strata.basics.currency.CurrencyAmountArray"}
stateful = {}
roots = {}
for line in Path(audit_output).read_text(encoding="utf-8").splitlines():
    entry = line.strip()
    if entry.startswith("FORGEABLE "):
        # `FORGEABLE <closed type> <chain>` - the chain being every class from a stateful
        # implementation up to the closed type, since the invariant belongs where the fields are.
        parts = entry[len("FORGEABLE "):].split(" ", 1)
        stateful[parts[0]] = parts[1].split(";") if len(parts) > 1 and parts[1] else [parts[0]]
    elif entry.startswith("ROOT "):
        # A level of a closed hierarchy: it declares no implementation and holds no data, so what
        # it runs is the subtype guard rather than the member guard of a named family - and it may
        # run it by inheritance, the guard belonging to the root of the hierarchy.
        parts = entry[len("ROOT "):].split(" ", 1)
        roots[parts[0]] = parts[1].split(";") if len(parts) > 1 and parts[1] else [parts[0]]

# The declaration headers, anchored at the start of a line so that a
# documentation line quoting the shape of a declaration is not read as one.
VALIDATED = re.compile(r"(?m)^\s*sealed abstract case class (\w+) private\b")
FAMILY = re.compile(r"(?m)^\s*sealed abstract class (\w+) private\[")

declarations = []
for source_root, classes_directory in ((collect_main, collect_classes), (basics_main, basics_classes)):
    for path in sorted(Path(source_root).rglob("*.scala")):
        text = path.read_text(encoding="utf-8")
        for kind, pattern, guard in (
            ("validated/normalising", VALIDATED, "requireSoleImplementation"),
            ("named family", FAMILY, "requireDeclaredMember"),
        ):
            for match in pattern.finditer(text):
                line = text[: match.start()].count("\n") + 1
                declarations.append((kind, match.group(1), guard, f"{path}:{line}", classes_directory))

# One index of the compiled classes per module, so the lookup below is a map
# read rather than a walk per declaration.
index = {}
for classes_directory in (collect_classes, basics_classes):
    names = {}
    root = Path(classes_directory)
    for path in root.rglob("*.class"):
        stem = path.name[: -len(".class")]
        binary = str(path.relative_to(root)).replace("/", ".")[: -len(".class")]
        # Keyed by the innermost name, so that a type declared inside another -
        # `DayCount.Bus252`, compiled to `DayCount$Bus252` - is found by the name
        # its declaration uses. A trailing `$` is the singleton class of an object.
        names.setdefault(stem.rstrip("$").split("$")[-1], []).append(binary)
    index[classes_directory] = names


def guards_in_constructor(binary_name):
    """The JvmClosure guards a class calls in its own constructor."""
    result = subprocess.run(
        ["javap", "-c", "-p", "-cp", classpath, binary_name],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    start = re.compile(r"^\s+(?:public |protected |private )?" + re.escape(binary_name) + r"\(")
    inside = False
    body = []
    for line in result.stdout.splitlines():
        if start.match(line):
            inside = True
        elif inside and not line.strip():
            inside = False
        if inside:
            body.append(line)
    return set(re.findall(r"JvmClosure\$\.(require\w+)", "\n".join(body)))


print("## the closed declarations of both modules, and the guard each calls")
missing = []
unstated = []
guarded = 0
invariant = 0
for kind, simple, guard, origin, classes_directory in declarations:
    candidates = [
        name
        for name in index[classes_directory].get(simple, [])
        if name.split(".")[-1].rstrip("$").split("$")[-1] == simple
    ]
    found = None
    found_calls = set()
    for binary_name in sorted(candidates, key=len):
        calls = guards_in_constructor(binary_name)
        if calls is None:
            # javap could not read the class: not the declaration this name refers to
            continue
        # A hierarchy level runs the subtype guard instead of the member guard, and is identified
        # by the reflective half rather than by its declaration, which is identical in shape.
        wanted = "requirePermittedSubtype" if binary_name in roots else guard
        if wanted not in calls and binary_name in roots:
            # inherited from the root of this hierarchy, which is the level that declares it
            if any(
                wanted in (guards_in_constructor(level) or set())
                for level in roots[binary_name]
                if level != binary_name
            ):
                calls = set(calls) | {wanted}
        if wanted in calls:
            found = binary_name
            found_calls = calls
            guard = wanted
            kind = "closed hierarchy level" if binary_name in roots else kind
            break
    if found is None:
        missing.append(f"{kind} {simple} ({origin}) does not call {guard} in its constructor")
        print(f"MISSING  {kind:22s} {simple:28s} {guard}  {origin}")
    else:
        guarded += 1
        # The invariant may be stated by the closed type itself or by any class between it and a
        # stateful implementation of it, because that is where the fields being checked live.
        states_invariant = "requireInvariant" in found_calls
        stated_on_chain = next(
            (
                step
                for step in stateful.get(found, [])
                if step != found and "requireInvariant" in (guards_in_constructor(step) or set())
            ),
            None,
        )
        if stated_on_chain is not None:
            states_invariant = True
        needs_invariant = found in stateful and found not in TOTAL_BY_CONSTRUCTION
        if needs_invariant and not states_invariant:
            unstated.append(
                f"{kind} {simple} ({origin}) carries state and states no invariant, so the "
                f"public constructor of its implementation can be handed any arguments"
            )
            print(f"NO-INVARIANT {kind:18s} {simple:28s} {guard}  {found}")
        else:
            if states_invariant:
                invariant += 1
            where = ""
            if states_invariant:
                where = "+requireInvariant"
                if stated_on_chain is not None:
                    where += "(" + stated_on_chain.split(".")[-1] + ")"
            print(f"guarded  {kind:22s} {simple:28s} {guard}{where}  {found}")

validated_count = sum(1 for entry in declarations if entry[0] == "validated/normalising")
family_count = len(declarations) - validated_count
print()
print(f"validated and normalising declarations = {validated_count}")
print(f"named family declarations             = {family_count}")
print(f"closed declarations                   = {len(declarations)}")
print(f"closed hierarchy levels among them     = {sum(1 for entry in declarations if entry[1] in {name.split('.')[-1] for name in roots})}")
print(f"guarded in constructor bytecode       = {guarded}")
print(f"stating an invariant over their fields = {invariant}")
print(f"stateful closed types requiring one    = {len(set(stateful) - TOTAL_BY_CONSTRUCTION)}")
print(f"closed types derived by the sweep     = {expected_closed}")
for entry in missing + unstated:
    print("   " + entry)

problems = list(missing) + list(unstated)
if len(stateful) < 30:
    problems.append(
        f"the reflective half reported only {len(stateful)} stateful closed types, so this half "
        f"would require an invariant of almost nothing"
    )
if invariant < len(set(stateful) - TOTAL_BY_CONSTRUCTION):
    problems.append(
        f"{invariant} of {len(set(stateful) - TOTAL_BY_CONSTRUCTION)} stateful closed types state an invariant"
    )
if validated_count < 30:
    problems.append(f"only {validated_count} validated or normalising declarations were found")
if family_count < 10:
    problems.append(f"only {family_count} named family declarations were found")
levels = sum(1 for entry in declarations if entry[1] in {name.split(".")[-1] for name in roots})
if str(len(declarations) - levels) != str(expected_closed):
    problems.append(
        f"the sources declare {len(declarations) - levels} closed types besides {levels} hierarchy "
        f"levels and the reflective sweep derived {expected_closed}: an implementation has moved "
        f"out of its family, or a type has lost its guard"
    )
if levels < 3:
    problems.append(f"only {levels} closed hierarchy levels were matched against the {len(roots)} derived")
if problems:
    print()
    print("FAIL: " + "; ".join(problems))
    sys.exit(1)
print()
print(
    "PASS: every closed declaration of both modules guards its own construction, and every one "
    "whose implementation carries state also states the invariant of its own fields"
)
PY
  cat "$sweep" >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "the source-derived guard sweep failed (exit $rc; see ${sweep#"$ROOT"/})"
    failed=1
  fi

  # -- 4. attack one: the hidden implementations cannot be named ----------
  local probe_log="$work/probe-javac.log"
  rc=0
  javac -cp "$classpath" -d "$work/probe-classes" "$work"/emit/probe/*.java \
    >"$probe_log" 2>&1 || rc=$?
  local probe_sources refused_names access_errors other_errors
  probe_sources="$(find "$work/emit/probe" -name '*.java' | awk 'NF { n++ } END { print n + 0 }')"
  refused_names="$(grep -o 'Probe_[0-9]*\.java' "$probe_log" |
    sort -u | awk 'NF { n++ } END { print n + 0 }')"
  access_errors="$(grep -c 'has private access' "$probe_log" || true)"
  # Every other diagnostic is a probe that failed for a reason this row does
  # not claim - a missing package, a syntax error - and is not evidence that
  # the implementation was unreachable.
  other_errors="$(grep -c 'error:' "$probe_log" || true)"
  other_errors=$((other_errors - access_errors))
  {
    printf '\n## attack one: naming a hidden implementation class from Java\n'
    printf '# javac exit status: %s (non-zero is the pass)\n' "$rc"
    printf '# probe sources: %s\n' "$probe_sources"
    printf '# probes javac refused: %s\n' "$refused_names"
    printf '# access errors: %s\n' "$access_errors"
    printf '# diagnostics of any other kind: %s (must be zero)\n' "$other_errors"
    command head -n 6 "$probe_log"
    printf '\n'
  } >>"$EV"
  if [[ "$rc" -eq 0 ]]; then
    detail "javac compiled a reference to a hidden implementation class"
    failed=1
  fi
  if [[ "$probe_sources" != "$probes" || "$probe_sources" -lt 30 ]]; then
    detail "the hidden-implementation probes are $probe_sources of $probes expected"
    failed=1
  fi
  if [[ "$refused_names" != "$probe_sources" ]]; then
    detail "javac refused $refused_names of $probe_sources hidden-implementation probes"
    failed=1
  fi
  if [[ "$access_errors" != "$probe_sources" ]]; then
    detail "$access_errors of $probe_sources probes were refused for the access of the implementation"
    failed=1
  fi
  if [[ "$other_errors" -ne 0 ]]; then
    detail "$other_errors probe diagnostic(s) were not about access: the probes did not compile as intended"
    failed=1
  fi

  # -- 5. attack two: an external subclass cannot be constructed ----------
  local forge_compile="$work/forge-javac.log"
  local forge_compile_rc=0
  javac -cp "$classpath" -d "$work/forge-classes" "$work"/emit/forge/*.java \
    >"$forge_compile" 2>&1 || forge_compile_rc=$?
  if [[ "$forge_compile_rc" -ne 0 ]]; then
    # The attack must COMPILE: a subclass javac refuses is an attack that was
    # never mounted, and the guard would then be untested rather than proven.
    detail "the generated external subclasses did not compile (javac exit $forge_compile_rc)"
    command head -n 10 "$forge_compile" >>"$EV"
    failed=1
  fi

  local forge_run="$work/forge-run.log"
  rc=0
  java -cp "$classpath:$work/forge-classes" audit.forge.ForgeRunner >"$forge_run" 2>&1 || rc=$?
  local guarded constructed attempted
  guarded="$(closure_metric 'refused by the construction guard' "$forge_run")"
  constructed="$(closure_metric 'constructed successfully' "$forge_run")"
  attempted="$(closure_metric 'forged subclasses' "$forge_run")"
  {
    printf '\n## attack two: constructing an external subclass of every closed type\n'
    printf '# javac exit status: %s (zero is expected - the JVM does permit the subclass)\n' \
      "$forge_compile_rc"
    printf '# runner exit status: %s\n' "$rc"
    cat "$forge_run"
    printf '\n'
  } >>"$EV"
  if [[ -z "$attempted" || "$attempted" != "$forges" || "$attempted" -lt 40 ]]; then
    detail "the forge attempted ${attempted:-no} subclasses of $forges generated"
    failed=1
  fi
  if [[ "$constructed" != "0" ]]; then
    detail "$constructed external subclass(es) of a closed type were constructed"
    failed=1
  fi
  if [[ -z "$guarded" || "$guarded" -lt 40 ]]; then
    detail "only ${guarded:-no} external subclasses were refused by the construction guard"
    failed=1
  fi
  if [[ "$rc" -ne 0 ]]; then
    detail "the forge runner reported a failure (exit $rc; see ${forge_run#"$ROOT"/})"
    failed=1
  fi


  # -- 6. attack three: a foreign subtype of a closed hierarchy level -----
  # The levels carry no data and declare no implementation, so parts 4 and 5
  # cannot see them - and they are the cheapest thing for a foreign class file
  # to claim, because until this port closed them they were traits, which
  # compile to plain JVM interfaces that anything may implement without running
  # a constructor. Each level is now an abstract class whose constructor refuses
  # a subtype outside the families it admits, so the subclass must compile and
  # must not construct.
  local root_compile="$work/rootforge-javac.log"
  local root_compile_rc=0
  javac -cp "$classpath" -d "$work/rootforge-classes" "$work"/emit/rootforge/*.java \
    >"$root_compile" 2>&1 || root_compile_rc=$?
  if [[ "$root_compile_rc" -ne 0 ]]; then
    detail "the generated foreign hierarchy subtypes did not compile (javac exit $root_compile_rc)"
    command head -n 10 "$root_compile" >>"$EV"
    failed=1
  fi

  local root_run="$work/rootforge-run.log"
  rc=0
  java -cp "$classpath:$work/rootforge-classes" audit.rootforge.RootForgeRunner \
    >"$root_run" 2>&1 || rc=$?
  local root_attempted root_guarded root_admitted
  root_attempted="$(closure_metric 'foreign subtypes of a hierarchy level' "$root_run")"
  root_guarded="$(closure_metric 'hierarchy levels refusing a foreign subtype' "$root_run")"
  root_admitted="$(closure_metric 'hierarchy levels admitting a foreign subtype' "$root_run")"
  {
    printf '\n## attack three: claiming a closed hierarchy level from outside\n'
    printf '# javac exit status: %s (zero is expected - the JVM does permit the subclass)\n' \
      "$root_compile_rc"
    printf '# runner exit status: %s\n' "$rc"
    cat "$root_run"
    printf '\n'
  } >>"$EV"
  if [[ -z "$root_attempted" || "$root_attempted" != "$root_forges" || "$root_attempted" -lt 3 ]]; then
    detail "the root forge attempted ${root_attempted:-no} subtypes of $root_forges generated"
    failed=1
  fi
  if [[ "$root_admitted" != "0" ]]; then
    detail "$root_admitted closed hierarchy level(s) admitted a foreign subtype"
    failed=1
  fi
  if [[ -z "$root_guarded" || "$root_guarded" -lt 3 ]]; then
    detail "only ${root_guarded:-no} hierarchy levels refused a foreign subtype through their guard"
    failed=1
  fi
  if [[ "$rc" -ne 0 ]]; then
    detail "the root forge runner reported a failure (exit $rc; see ${root_run#"$ROOT"/})"
    failed=1
  fi

  # -- 7. attack four: calling the binary constructors directly -----------
  # Part 4 proves that `javac` will not NAME a hidden implementation, which is
  # the `InnerClasses` attribute being honoured and nothing more. This part
  # mounts the attack that attribute does not stop. A stub tree declares each
  # implementation as an ordinary top-level class under its BINARY name - `$` is
  # a legal Java identifier character, so `CurrencyAmount$Impl` is nameable as a
  # top-level type - with the constructor signature the real class file
  # publishes. The attacker is compiled against that stub, which makes its
  # bytecode a plain `new` and `invokespecial` on the real binary constructor,
  # and is then RUN against the real classes, where those instructions resolve
  # to the implementation itself. It is the emitted-bytecode route, not a
  # source-level probe, and every forged state must be refused by the invariant
  # the type states over its own fields.
  local binary_stub="$work/binary-stub"
  local binary_attack="$work/binary-attack"
  mkdir -p "$binary_stub/com/opengamma/strata/collect" \
    "$binary_stub/com/opengamma/strata/basics" \
    "$binary_stub/com/opengamma/strata/basics/currency" \
    "$binary_stub/com/opengamma/strata/basics/date" \
    "$binary_stub/com/opengamma/strata/basics/location" \
    "$binary_stub/com/opengamma/strata/basics/schedule" \
    "$binary_stub/com/opengamma/strata/basics/value" \
    "$binary_attack/atk" || failed=1
  printf 'package com.opengamma.strata.collect;\npublic final class Decimal$Impl { public Decimal$Impl(long unscaled, int scale) {} }\n' \
    >"$binary_stub/com/opengamma/strata/collect/Decimal\$Impl.java"
  printf 'package com.opengamma.strata.basics;\npublic final class StandardId$Impl { public StandardId$Impl(String scheme, String value) {} }\n' \
    >"$binary_stub/com/opengamma/strata/basics/StandardId\$Impl.java"
  printf 'package com.opengamma.strata.basics.currency;\npublic final class Currency$Impl { public Currency$Impl(String code, int minorUnitDigits, String triangulationCode) {} }\n' \
    >"$binary_stub/com/opengamma/strata/basics/currency/Currency\$Impl.java"
  printf 'package com.opengamma.strata.basics.date;\nimport java.time.Period;\npublic final class Tenor$Impl { public Tenor$Impl(Period period, String name) {} }\n' \
    >"$binary_stub/com/opengamma/strata/basics/date/Tenor\$Impl.java"
  printf 'package com.opengamma.strata.basics.location;\npublic final class Country$Impl { public Country$Impl(String code) {} }\n' \
    >"$binary_stub/com/opengamma/strata/basics/location/Country\$Impl.java"
  printf 'package com.opengamma.strata.basics.schedule;\nimport java.time.LocalDate;\npublic final class SchedulePeriod$Impl { public SchedulePeriod$Impl(LocalDate unadjustedStart, LocalDate unadjustedEnd, LocalDate start, LocalDate end) {} }\n' \
    >"$binary_stub/com/opengamma/strata/basics/schedule/SchedulePeriod\$Impl.java"
  printf 'package com.opengamma.strata.basics.value;\npublic final class HalfUp$Impl { public HalfUp$Impl(int decimalPlaces, int fraction) {} }\n' \
    >"$binary_stub/com/opengamma/strata/basics/value/HalfUp\$Impl.java"

  if ! cat >"$binary_attack/atk/BinaryConstructors.java" <<'JAVA'
package atk;

import com.opengamma.strata.basics.StandardId$Impl;
import com.opengamma.strata.basics.currency.Currency$Impl;
import com.opengamma.strata.basics.date.Tenor$Impl;
import com.opengamma.strata.basics.location.Country$Impl;
import com.opengamma.strata.basics.schedule.SchedulePeriod$Impl;
import com.opengamma.strata.basics.value.HalfUp$Impl;
import com.opengamma.strata.collect.Decimal$Impl;
import java.time.LocalDate;
import java.time.Period;

/**
 * Calls the binary constructor of a hidden implementation directly, with state no factory of the
 * library would produce.
 *
 * Compiled against a stub tree that declares each implementation under its binary name, and run
 * against the real classes - so each `new` below is an `invokespecial` on the implementation's own
 * `ACC_PUBLIC` constructor, reached without a Scala compiler and without the `InnerClasses`
 * attribute being consulted. Every one must be refused, and refused by the type's invariant
 * rather than by chance: the states chosen are the ones the factories reject.
 */
public final class BinaryConstructors {

  private static int refused = 0;
  private static int otherwise = 0;
  private static int constructed = 0;

  private interface Forgery {
    Object construct();
  }

  private static void attempt(String label, Forgery forgery) {
    try {
      Object value = forgery.construct();
      constructed++;
      System.out.println("CONSTRUCTED " + label + " -> " + value);
    } catch (IllegalArgumentException refusal) {
      String message = refusal.getMessage();
      if (message != null && message.contains("a value of this type requires that")) {
        refused++;
        System.out.println("REFUSED-BY-INVARIANT " + label + " -> " + message);
      } else {
        otherwise++;
        System.out.println("REFUSED-OTHERWISE " + label + " -> " + message);
      }
    } catch (Throwable other) {
      otherwise++;
      System.out.println("REFUSED-OTHERWISE " + label + " -> " + other);
    }
  }

  public static void main(String[] args) {
    attempt("Decimal holding a scale beyond the representation",
        () -> new Decimal$Impl(1L, 99));
    attempt("Currency naming a code the reference data does not publish",
        () -> new Currency$Impl("ZZZ", 9, "USD"));
    attempt("Currency naming a published code with the wrong minor units",
        () -> new Currency$Impl("GBP", 9, "USD"));
    attempt("StandardId holding an empty scheme",
        () -> new StandardId$Impl("", "AAPL"));
    attempt("Country holding a lower-case code",
        () -> new Country$Impl("gb"));
    attempt("Country holding a code of the wrong length",
        () -> new Country$Impl("GBR"));
    attempt("SchedulePeriod holding dates that run backwards",
        () -> new SchedulePeriod$Impl(
            LocalDate.of(2014, 4, 1), LocalDate.of(2014, 1, 1),
            LocalDate.of(2014, 4, 1), LocalDate.of(2014, 1, 1)));
    attempt("Rounding holding a fraction no rounding represents",
        () -> new HalfUp$Impl(4, 1));
    attempt("Tenor holding a name its own period does not imply",
        () -> new Tenor$Impl(Period.ofMonths(3), "1D"));
    attempt("Tenor holding a period no tenor covers",
        () -> new Tenor$Impl(Period.ZERO, "0D"));
    System.out.println("binary constructions attempted = " + (refused + otherwise + constructed));
    System.out.println("refused by the invariant of the type = " + refused);
    System.out.println("refused for another reason = " + otherwise);
    System.out.println("constructed successfully = " + constructed);
    System.exit(constructed == 0 && refused >= 10 ? 0 : 1);
  }
}
JAVA
  then
    detail "the binary-constructor attack source could not be written"
    failed=1
  fi

  local binary_log="$work/binary-javac.log"
  local binary_rc=0
  # The stub sources, collected NUL-delimited into an array rather than left
  # to word-splitting. `javac -d dir $(find ...)` compiles the right files
  # only while every path in the result is free of whitespace, and these paths
  # start at the checkout root - so an operator whose checkout sits in a
  # directory with a space in its name would have seen this row fail for a
  # reason that has nothing to do with what it measures. `-print0` and a
  # `read -d ''` loop carry each name whole (shellcheck SC2046).
  local -a binary_stub_sources=()
  local stub_source
  while IFS= read -r -d '' stub_source; do
    binary_stub_sources+=("$stub_source")
  done < <(find "$binary_stub" -name '*.java' -print0)
  if [[ "${#binary_stub_sources[@]}" -eq 0 ]]; then
    detail "the binary-constructor attack source was not found where it was written"
    failed=1
    binary_rc=1
  else
    # The stub is compiled WITHOUT the real classes on the classpath: the point
    # is a compilation unit that declares the binary names itself, so that the
    # attacker's bytecode carries them and the real class files answer for them
    # at run time.
    javac -d "$work/binary-stub-classes" "${binary_stub_sources[@]}" \
      >"$binary_log" 2>&1 || binary_rc=$?
  fi
  if [[ "$binary_rc" -eq 0 ]]; then
    javac -cp "$work/binary-stub-classes" -d "$work/binary-attack-classes" \
      "$binary_attack/atk/BinaryConstructors.java" >>"$binary_log" 2>&1 || binary_rc=$?
  fi
  if [[ "$binary_rc" -ne 0 ]]; then
    detail "the binary-constructor attack did not compile (javac exit $binary_rc)"
    command head -n 10 "$binary_log" >>"$EV"
    failed=1
  fi

  # The bytecode is shown in the evidence, because the claim of this part is
  # about the instructions and not about the source they came from.
  local binary_bytecode="$work/binary-bytecode.txt"
  javap -c -p -cp "$work/binary-attack-classes" atk.BinaryConstructors 2>/dev/null \
    | command grep -E '^[[:space:]]+[0-9]+: (new|invokespecial)' \
    | command grep 'Impl' >"$binary_bytecode" || true
  local binary_instructions
  binary_instructions="$(command wc -l <"$binary_bytecode" | tr -d ' ')"

  local binary_run="$work/binary-run.log"
  rc=0
  java -cp "$work/binary-attack-classes:$classpath" atk.BinaryConstructors >"$binary_run" 2>&1 || rc=$?
  local binary_attempted binary_refused binary_built
  binary_attempted="$(closure_metric 'binary constructions attempted' "$binary_run")"
  binary_refused="$(closure_metric 'refused by the invariant of the type' "$binary_run")"
  binary_built="$(closure_metric 'constructed successfully' "$binary_run")"
  {
    printf '\n## attack four: invoking the binary constructor of a hidden implementation\n'
    printf '# javac exit status: %s (zero is expected - the attacker names the binary class itself)\n' \
      "$binary_rc"
    printf '# new/invokespecial instructions on an implementation class: %s\n' "$binary_instructions"
    command sed -n '1,8p' "$binary_bytecode"
    printf '# runner exit status: %s\n' "$rc"
    cat "$binary_run"
    printf '\n'
  } >>"$EV"
  if [[ -z "$binary_instructions" || "$binary_instructions" -lt 14 ]]; then
    detail "the binary-constructor attack emitted ${binary_instructions:-no} instructions naming an implementation"
    failed=1
  fi
  if [[ -z "$binary_attempted" || "$binary_attempted" -lt 10 ]]; then
    detail "the binary-constructor attack attempted ${binary_attempted:-no} constructions"
    failed=1
  fi
  if [[ "$binary_built" != "0" ]]; then
    detail "$binary_built forged value(s) were constructed through a binary constructor"
    failed=1
  fi
  if [[ -z "$binary_refused" || "$binary_refused" != "$binary_attempted" ]]; then
    detail "${binary_refused:-no} of $binary_attempted binary constructions were refused by the type's invariant"
    failed=1
  fi
  if [[ "$rc" -ne 0 ]]; then
    detail "the binary-constructor attack reported a failure (exit $rc; see ${binary_run#"$ROOT"/})"
    failed=1
  fi

  # -- 8. attack five: a forged object stream -----------------------------
  # The refusal of Java serialization is two inherited hooks, and a subclass
  # that overrode them - returning itself instead of refusing - would be read
  # back from a stream with its fields populated and no constructor run. Both
  # are declared `final`, so the compiler emits them `ACC_FINAL` and the JVM
  # rejects such a class when it is LOADED. That is mounted here rather than
  # asserted: a stub declares the type with non-final hooks, a subclass
  # overriding both is compiled against it, a real object stream carrying that
  # subclass is written while the stub is on the classpath, and the stream is
  # then read against the real classes, where `readObject` itself must fail.
  local stream_stub="$work/stream-stub"
  local stream_attack="$work/stream-attack"
  mkdir -p "$stream_stub/com/opengamma/strata/collect" "$stream_attack/attack" || failed=1
  if ! cat >"$stream_stub/com/opengamma/strata/collect/Decimal.java" <<'JAVA'
package com.opengamma.strata.collect;

/**
 * A deliberately lying stub of the real type: it declares the two serialization hooks
 * OVERRIDABLE, which the real class does not, so that a subclass overriding them compiles.
 */
public abstract class Decimal implements java.io.Serializable {

  private static final long serialVersionUID = 1L;

  protected Decimal() {
  }

  protected Object writeReplace() {
    return this;
  }

  protected Object readResolve() {
    return this;
  }
}
JAVA
  then
    detail "the forged-stream stub could not be written"
    failed=1
  fi
  if ! cat >"$stream_attack/attack/ForgedDecimal.java" <<'JAVA'
package attack;

import com.opengamma.strata.collect.Decimal;

/** A subclass that overrides both refusal hooks, so that a stream can carry it. */
public final class ForgedDecimal extends Decimal {

  private static final long serialVersionUID = 1L;

  private final int forgedScale = 99;

  @Override
  protected Object writeReplace() {
    return this;
  }

  @Override
  protected Object readResolve() {
    return this;
  }

  @Override
  public String toString() {
    return "ForgedDecimal(scale=" + forgedScale + ")";
  }
}
JAVA
  then
    detail "the forged-stream subclass could not be written"
    failed=1
  fi
  if ! cat >"$stream_attack/attack/WriteForgedStream.java" <<'JAVA'
package attack;

import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;

/** Writes a real object stream carrying the forged subclass, with the stub on the classpath. */
public final class WriteForgedStream {

  public static void main(String[] args) throws Exception {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (ObjectOutputStream out = new ObjectOutputStream(bytes)) {
      out.writeObject(new ForgedDecimal());
    }
    try (FileOutputStream file = new FileOutputStream(args[0])) {
      file.write(bytes.toByteArray());
    }
    System.out.println("forged stream bytes written = " + bytes.size());
  }
}
JAVA
  then
    detail "the forged-stream writer could not be written"
    failed=1
  fi
  if ! cat >"$stream_attack/attack/ReadForgedStream.java" <<'JAVA'
package attack;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;

/**
 * Reads the forged stream against the REAL classes.
 *
 * `ObjectInputStream.readObject` must fail before it returns a value: resolving the stream's class
 * loads the forged subclass, and the JVM rejects a class that overrides a final method.
 */
public final class ReadForgedStream {

  public static void main(String[] args) throws Exception {
    byte[] bytes;
    try (FileInputStream file = new FileInputStream(args[0])) {
      bytes = file.readAllBytes();
    }
    System.out.println("forged stream bytes read = " + bytes.length);
    try (ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
      Object restored = in.readObject();
      System.out.println("CONSTRUCTED " + restored.getClass().getName() + " -> " + restored);
      System.out.println("forged streams refused = 0");
      System.exit(1);
    } catch (Throwable refusal) {
      System.out.println("REFUSED " + refusal.getClass().getName() + ": " + refusal.getMessage());
      System.out.println("forged streams refused = 1");
      System.exit(0);
    }
  }
}
JAVA
  then
    detail "the forged-stream reader could not be written"
    failed=1
  fi

  local stream_log="$work/stream-javac.log"
  local stream_rc=0
  javac -d "$work/stream-stub-classes" "$stream_stub/com/opengamma/strata/collect/Decimal.java" \
    >"$stream_log" 2>&1 || stream_rc=$?
  if [[ "$stream_rc" -eq 0 ]]; then
    javac -cp "$work/stream-stub-classes" -d "$work/stream-attack-classes" \
      "$stream_attack"/attack/*.java >>"$stream_log" 2>&1 || stream_rc=$?
  fi
  if [[ "$stream_rc" -ne 0 ]]; then
    detail "the forged-stream attack did not compile (javac exit $stream_rc)"
    command head -n 10 "$stream_log" >>"$EV"
    failed=1
  fi

  local stream_write="$work/stream-write.log"
  local stream_read="$work/stream-read.log"
  local write_rc=0
  local read_rc=0
  java -cp "$work/stream-attack-classes:$work/stream-stub-classes" \
    attack.WriteForgedStream "$work/forged.ser" >"$stream_write" 2>&1 || write_rc=$?
  java -cp "$work/stream-attack-classes:$classpath" \
    attack.ReadForgedStream "$work/forged.ser" >"$stream_read" 2>&1 || read_rc=$?
  local stream_bytes streams_refused
  stream_bytes="$(closure_metric 'forged stream bytes written' "$stream_write")"
  streams_refused="$(closure_metric 'forged streams refused' "$stream_read")"
  {
    printf '\n## attack five: reading a forged object stream against the real classes\n'
    printf '# javac exit status: %s (zero is expected - the subclass compiles against the stub)\n' \
      "$stream_rc"
    printf '# writer exit status: %s (the stream is written with the stub on the classpath)\n' "$write_rc"
    cat "$stream_write"
    printf '# reader exit status: %s (the stream is read with the real classes)\n' "$read_rc"
    cat "$stream_read"
    printf '\n'
  } >>"$EV"
  if [[ "$write_rc" -ne 0 || -z "$stream_bytes" || "$stream_bytes" -lt 50 ]]; then
    detail "the forged stream was not written (writer exit $write_rc, ${stream_bytes:-no} bytes): the attack was never mounted"
    failed=1
  fi
  if [[ "$streams_refused" != "1" || "$read_rc" -ne 0 ]]; then
    detail "the forged object stream was not refused on the read path (reader exit $read_rc)"
    failed=1
  fi

  add_appendix "JVM construction and serialization closure" <"$audit_output" || failed=1

  if [[ "$failed" -eq 0 ]]; then
    detail "$products products all refuse Java serialization, on hooks no subclass can override; $closed_types closed types all guard their construction and every stateful one states its own invariant; $probes hidden implementations unnameable from Java and $entry_points reachable in bytecode, of which $binary_attempted forged constructions were all refused; $attempted external subclasses and $root_attempted foreign subtypes of a hierarchy level compiled and none constructed; the forged object stream was refused on the read path"
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
#     actually read, and an entry for which no such class can be found FAILS
#     this row.
#
#     A hidden class cannot be disassembled under its own name, and neither
#     can the marker the JVM records in place of a source: `source:
#     __JVM_LookupDefineClass__` is not a class name, and handing it to javap
#     would fail the row on a class that was never there. The host is
#     therefore derived in a fixed order - the `source:` field when it names a
#     real class (a lambda records its host there), else the identity up to
#     `$$Lambda`, else the enclosing class of the identity - and the
#     disassembly of that host is then attempted from its recorded source,
#     from the runtime image, and from this build's own output directories in
#     turn.
#
#     What is NOT allowed is an entry nobody read. An entry with no derivable
#     host, and an entry whose host none of those attempts can disassemble,
#     are each a failure of this row with their count in its detail and each
#     entry named in the evidence: the reflection-package check above is made
#     on names, and a name is not bytecode, so accepting such an entry would
#     leave the code that actually ran unexamined.
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

# Disassembles one class, trying in turn every place it can legitimately be
# read from. $1 class name, $2 recorded source; prints the disassembly and
# returns 0, or returns 1 having read nothing.
#
# One attempt is not a conclusion. A host derived from a hidden class's name
# is not the class whose source the log recorded, a class of the JDK's own
# image has no source to name, and a class of this build may have been loaded
# from a directory the log wrote in another form. So the recorded source is
# tried first, then the ordinary lookup with no classpath (the runtime image
# and the CDS archive), then this build's four output directories. Each
# attempt is recorded in rule6-javap-attempts.txt, so the evidence says which
# one produced the bytes that were grepped.
javap_best_effort() {
  local class_name="$1"
  local source="$2"
  local attempts="$AUDIT_DIR/rule6-javap-attempts.txt"
  local out=""
  local rc=0

  out="$(javap_from_source "$class_name" "$source" 2>&1)" || rc=$?
  if [[ "$rc" -eq 0 && -n "$out" ]]; then
    printf '%s\tread from its recorded source (%s)\n' "$class_name" "$source" >>"$attempts"
    printf '%s\n' "$out"
    return 0
  fi

  rc=0
  out="$(javap -c -p "$class_name" 2>&1)" || rc=$?
  if [[ "$rc" -eq 0 && -n "$out" ]]; then
    printf '%s\tread from the runtime image, no classpath\n' "$class_name" >>"$attempts"
    printf '%s\n' "$out"
    return 0
  fi

  rc=0
  out="$(javap -c -p -cp \
    "$COLLECT_CLASSES:$BASICS_CLASSES:$COLLECT_TEST_CLASSES:$BASICS_TEST_CLASSES" \
    "$class_name" 2>&1)" || rc=$?
  if [[ "$rc" -eq 0 && -n "$out" ]]; then
    printf '%s\tread from the output directories of this build\n' "$class_name" >>"$attempts"
    printf '%s\n' "$out"
    return 0
  fi

  printf '%s\tUNREADABLE by every attempt (recorded source: %s)\n' "$class_name" "$source" \
    >>"$attempts"
  return 1
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
      printf '# (a)\n'
      command_line 'find strata-collect/target strata-basics/target -path "*scala-2.13/classes/*.class" -exec javap -c -p {} +'
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

  local mode rc log_option
  for mode in baseline codec; do
    rc=0
    # The log path crosses from shell into SCALA SOURCE here - sbt compiles
    # and evaluates the `set` command - so it is encoded as a Scala string
    # literal instead of being interpolated raw into one. AAP section 0.10.1
    # requires the log under target/audit, so the path itself is unchanged.
    if ! log_option="$(scala_string_literal "-Xlog:class+load:file=$AUDIT_DIR/$mode-classload.log")"; then
      detail "(b) the $mode class-load log path cannot be encoded as a Scala string literal, so that audit run was not started"
      printf '# (b) %s run: NOT STARTED - its class-load log path could not be encoded\n' "$mode" >>"$EV"
      failed=1
      continue
    fi
    run_sbt "rule6-audit-$mode" \
      "set \`strata-basics\` / Test / javaOptions ++= Seq(\"-Dcodec.audit=$mode\", $log_option)" \
      "strata-basics/testOnly com.opengamma.strata.basics.json.JsonRoundTripSpec" || rc=$?
    local digest
    digest="$(strip_sbt_prefix <"$SBT_LOG" | awk '/^CODEC-AUDIT-DIGEST / { print; exit }')"
    {
      command_line "sbt -batch \"set \\\`strata-basics\\\` / Test / javaOptions ++= Seq(\\\"-Dcodec.audit=$mode\\\", \\\"-Xlog:class+load:file=<audit>/$mode-classload.log\\\")\" \"strata-basics/testOnly com.opengamma.strata.basics.json.JsonRoundTripSpec\""
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
  # which no class can be named to disassemble - which the row fails on,
  # rather than accepting them as documented.
  local adjudication="$AUDIT_DIR/rule6-delta-adjudication.txt"
  local identities="$AUDIT_DIR/rule6-delta-identities.txt"
  local inspection="$AUDIT_DIR/rule6-delta-inspection-list.txt"
  local noninspectable="$AUDIT_DIR/rule6-unaudited-entries.txt"
  local classification_rc=0
  python3 - "$delta" "$baseline_names" "$codec_names" "$combined_map" \
    "$adjudication" "$identities" "$inspection" "$noninspectable" <<'PY' >>"$EV" 2>&1 || classification_rc=$?
import re
import sys
from collections import Counter

(delta_path, baseline_path, codec_path, map_path, adjudication_path, identities_path,
 inspection_path, noninspectable_path) = sys.argv[1:9]

HIDDEN = re.compile(r"/0x[0-9a-fA-F]+$")
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

    Returning nothing is a failure of the row, not an exemption, so every step
    that can legitimately name a host is taken - including the outer class of
    an identity that has no package at all.

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
    if outer and outer != simple:
        enclosing = f"{package}.{outer}" if package else outer
        return enclosing, "the enclosing class of a JVM-defined hidden class", "jvm"
    return (None,
            f"no host class can be derived to disassemble (source: {source or 'unknown'})",
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
    #   charged   an ordinary class of the difference belonging to an
    #             application module or one of its libraries; or a lambda
    #             whose host class is itself absent from the baseline run, so
    #             the body being read exists only because encode/decode ran.
    #             A reflection reference in it is a reflection reference on
    #             the codec path.
    #   reported  a PLATFORM class of the JDK. Its body is the runtime's own
    #             implementation rather than application code:
    #             `java.util.Random` reads a field offset through
    #             `Class.getDeclaredField` to seed itself, and decoding a date
    #             pulls in a dozen `java.time.format` classes, so charging
    #             those bodies would fail this row for using
    #             `LocalDate.parse`. What it cannot hide is application code
    #             reflecting: that loads classes of `java.lang.reflect` and
    #             friends, and (b) above fails on any of those appearing in
    #             the difference, platform or not.
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
        noninspectable.append(f"{name}\tUNAUDITED ({why})")
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
print(f"#       entries with no host to disassemble, each a failure: {len(noninspectable)}")
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
  # An entry counted here is an entry whose bytes nobody read, which is a
  # failure of this row: a name that passed the reflection-package check is
  # not a body that was grepped.
  local hostless
  hostless="$(awk 'NF { n++ } END { print n + 0 }' "$noninspectable")"
  if [[ "${hostless:-0}" -ne 0 ]]; then
    detail "(c) $hostless entry(ies) of the difference have no class to disassemble, so their bytecode is unaudited"
    failed=1
  fi

  # The record of where each class was read from, started empty so a previous
  # run's attempts cannot be read as this one's.
  if ! safe_truncate "$AUDIT_DIR/rule6-javap-attempts.txt"; then
    detail "(c) the javap attempt log could not be started"
    failed=1
  fi

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
    out="$(javap_best_effort "$target" "$source" 2>&1)" || inspect_rc=$?
    if [[ "$inspect_rc" -ne 0 || -z "$out" ]]; then
      # Nothing was read, from the recorded source or from anywhere else, so
      # these entries of the difference are unaudited. Both kinds fail the
      # row - an ordinary class of the difference and a host derived for a
      # hidden one alike - because the bytes that ran are what this row is
      # about, and the identity check above was made on a name.
      printf '%s\t%s\t%s\tUNREADABLE by every attempt (status %s, recorded source %s)\t%s entry(ies)\n' \
        "$charge" "$bucket" "$target" "$inspect_rc" "$source" "$entries" >>"$per_class"
      if [[ "$bucket" == "plain" ]]; then
        detail "(c) no readable bytecode for the ordinary class $target, covering $entries entry(ies)"
        unreadable=$((unreadable + entries))
      else
        printf '%s\tUNAUDITED (the derived host could not be disassembled by any attempt, status %s) - %s entry(ies)\n' \
          "$target" "$inspect_rc" "$entries" >>"$noninspectable"
        detail "(c) no readable bytecode for the derived host $target, covering $entries entry(ies)"
        undisassemblable=$((undisassemblable + entries))
      fi
      failed=1
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
    printf '#     entries with no host to read, unaudited (a failure): %s\n' "$hostless"
    printf '#     entries whose derived host no attempt could disassemble (a failure): %s\n' \
      "$undisassemblable"
    printf '#     entries of an ordinary class javap could not read (a failure): %s\n' "$unreadable"
    printf '#     entries accounted for: %s of %s\n' "$accounted" "$RULE6_DELTA_SIZE"
    printf '#     reflection references charged to encode/decode: %s\n' "$total_hits"
    printf '#     reflection references reported but not charged (pre-existing identities and\n'
    printf '#       the JVM'"'"'s own method-handle machinery): %s\n' "$reported_hits"
    printf '#     per-class detail: %s\n' "${per_class#"$ROOT"/}"
    printf '#     unaudited entries: %s\n' "${noninspectable#"$ROOT"/}"
    printf '#     where each class was read from: %s\n' \
      "${AUDIT_DIR#"$ROOT"/}/rule6-javap-attempts.txt"
  } >>"$EV"

  # Reconciliation: every entry of the raw difference was inspected through a
  # class, or is one of the unaudited entries already failed above. An entry
  # that is neither would have left the audit silently, which is the whole
  # failure mode this row is built to prevent.
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
    printf 'unaudited (each a failure): %s entries (%s with no host, %s whose host no attempt\n' \
      "$((hostless + undisassemblable))" "$hostless" "$undisassemblable"
    printf '                           could disassemble)\n'
    printf 'entries accounted for    : %s of %s\n' "$accounted" "$RULE6_DELTA_SIZE"
    printf 'reflection references    : %s charged, %s reported (a paired occurrence, a host the\n' \
      "$total_hits" "$reported_hits"
    printf '                           baseline run loaded too, or the JVM'"'"'s own machinery)\n'
    printf '\nunaudited entries, with the reason each was recorded under (empty is the pass):\n'
    cat "$noninspectable"
    printf '\nwhere each disassembled class was read from:\n'
    if [[ -f "$AUDIT_DIR/rule6-javap-attempts.txt" ]]; then
      cat "$AUDIT_DIR/rule6-javap-attempts.txt"
    else
      printf '(the attempt log was never started)\n'
    fi
  } | add_appendix "Rule 6 - class-load audit of the codec path"

  if [[ "$failed" -eq 0 ]]; then
    detail "no reflection in either module; every one of the $RULE6_DELTA_SIZE entries of the encode/decode difference was read through a disassembled class and holds none either"
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
  {
    printf '## Rule 8 - class-file major version 65 (JVM 21)\n'
    command_line 'javap -v over strata-basics .../currency/Currency.class and strata-collect .../array/DoubleArray.class, reading the major version line of each'
    printf '\n'
  } >>"$EV"

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
# flag is a hit like any other, which is why no source in either module names
# it: a scaladoc sentence about the option is a failure of this row.
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
# suppression, which silences a lint key for a whole scope); and a positive
# control that the raw scan really did find build.sbt's option line, so the row
# cannot pass because its scope or its pattern silently stopped matching
# anything.
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
    command_line "grep -n '\"-Werror\"' build.sbt"
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
    command_line "grep -rn -E \"nowarn|SuppressWarnings|-Wconf|Werror\" build.sbt ${build_definition[*]} strata-collect/src strata-basics/src"
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
    command_line 'sbt -batch clean compile Test/compile'
    printf '# sbt exit status: %s\n' "$rc"
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
    command_line 'find strata-collect/target strata-basics/target -path "*scala-2.13/classes/*.class" -exec javap -s -protected {} +'
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
    command_line 'sbt -batch "strata-basics/run"'
    printf '# sbt exit status: %s\n' "$rc"
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

# gate7_manual_detail <verdict of the automated Gate 7 row> - the detail cell
# of the reported manual-approval row.
#
# The specification prescribes what this row says, verbatim, so
# GATE7_MANUAL_TEXT is printed exactly as written and always: a reader or a
# tool looking for that sentence finds it on every run, whatever happened.
# What it must not do is ASSERT something the row above it contradicts. The
# sentence opens with "automated checks passed", and when the automated half
# of Gate 7 has just FAILED, a reader of the table was being told by one row
# that the checks passed and by the row above it that they did not - with no
# way to tell which statement was measured. So the prescribed sentence is
# followed by a qualifier naming the automated row's actual verdict whenever
# it is not a pass. Nothing is concealed either way: the automated half is its
# own PASS/FAIL row immediately above, counted in the summary line and in the
# exit status.
gate7_manual_detail() {
  local automated_status="${1:-absent}"

  if [[ "$automated_status" == "PASS" ]]; then
    printf '%s\n' "$GATE7_MANUAL_TEXT"
    return 0
  fi
  printf '%s [QUALIFIED: the sentence before this one is the wording the specification prescribes for this row; the automated half of Gate 7 in the row above is %s on this run, so it describes the state required for approval and not the state observed]\n' \
    "$GATE7_MANUAL_TEXT" "$automated_status"
}

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
    command_line "grep -c \"^## \" $note"
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

  {
    printf '## Test scope at least equal to the Java suites\n'
    command_line 'python3 (the traceability join): sum the tests attribute of every testsuite in target/audit/snapshot/test-reports/TEST-*.xml by module, then join strata-basics/src/test/resources/manifest/java-test-mapping.csv against the @Test/@ParameterizedTest methods under modules/basics/src/test/java and modules/collect/src/test/java and against the testcase elements of that XML'
    printf '\n'
  } >>"$EV"

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

# The twenty Java collect test classes that map to Scala specs, named here so
# the authoritative inventory is this reviewed script and never the manifest
# under audit: deriving the expected set from the audited file would let an
# omitted class hide every one of its methods.
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
# subjects are members of collect `Collections.scala` map to `CollectionsSpec`
# - so a class-wide exception would let any of those thirty-four be retired to
# `partial` and its lost coverage pass unnoticed. Pinning the exact pairs whose
# subjects are outside the module's scope (the Guavate and MapStream members
# `strata-basics` does not use) closes it in both directions: a pair outside
# this list cannot be excused, and each of the thirty-four in the complement
# has no status left but `ported` or `consolidated:<spec>`, both of which must
# join to a JUnit test case below. Every pin is checked against the Java
# sources further down, so a stale entry cannot widen the permission either.
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
# THE COMMITTED STATE, WHICH THE STATUS ALONE DOES NOT COVER
#   `git status` describes the working tree against HEAD, so a change to the
#   Maven tree that has been COMMITTED is invisible to it: the tree is dirty
#   for exactly as long as it takes to commit, and a clean status afterwards
#   says nothing about what HEAD contains. The Java modules are the baseline
#   authority of this delivery - the parity fixtures and the reference-data
#   manifest are captured from them - so a committed edit there is the one
#   change that could make every parity row agree with a moved baseline.
#
#   Two checks close that, and both run:
#
#     pinned content  the git object id of each protected path at HEAD,
#                     compared against the id recorded in PROTECTED_PATH_IDS
#                     below. A tree or blob id IS the content of that path, so
#                     equality is proof that nothing under it changed, in any
#                     commit, on any branch, with no ref or network needed.
#                     The pin lives in this reviewed script, which is what
#                     makes it a baseline rather than a reading of the state
#                     it is supposed to be measuring. Changing the Java tree
#                     deliberately means changing the pin deliberately, in a
#                     reviewed diff.
#     ancestry        when a baseline ref can be resolved (origin/main,
#                     origin/master, main, master, in that order), it must be
#                     an ancestor of HEAD - a rewritten or unrelated history
#                     is a failure, not a pass - and the diff from the merge
#                     base to HEAD over the protected paths must be empty.
#                     A ref that resolves and disagrees fails the row; a
#                     checkout with none of those refs is reported, and the
#                     pinned ids above still decide the row.
#
# THE SECRET SCAN, AND WHY IT IS HERE
#   This run produces four evidence paths - `target/gate-report.md`,
#   `target/parity-report`, `target/test-reports` and `target/audit` - and the
#   audit tree holds every sbt log, every class-load log and every row's
#   evidence. Publishing an artifact is a disclosure, so the run checks its
#   own output for credential signatures BEFORE anything derived from it
#   leaves the machine. This is the last row, so by the time it executes every
#   other row has finished writing, which is what makes it the right place for
#   the scan of the ORIGINALS.
#
#   It is not the only time the scan runs. The upload is a separate tree -
#   `target/publish`, built by `finalize_publication` from sanitized copies - and
#   every phase below scans something different:
#
#     pre-run          before the first row, over whatever an earlier run
#                      left behind. A finding there stops the run and moves
#                      the offending artifact into the quarantine, and a
#                      clean tree with nothing in it is not a failure.
#     publication      this row: the four evidence paths as they stand, with
#                      the three trees the run produces required to exist.
#     staged           the copies that will actually be uploaded, scanned
#                      after they are sanitized; a match is withdrawn from
#                      the publication tree and its original quarantined.
#     assembly         the composed report, scanned BEFORE it is renamed into
#                      place, so a finding is recorded as a blocking row and
#                      the report is reassembled without its appendices
#                      rather than published as it stood.
#     composed-report  the report file that is on disk, before a sanitized
#                      copy of it is staged.
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

# The four evidence paths a run produces, in the order they are staged for
# publication. They are what the scans below examine; they are NOT what CI
# uploads. CI uploads `target/publish`, the scan-approved tree
# `finalize_publication` builds from these four - because detecting a credential
# in a path that has already been handed to `store_artifacts` changes nothing
# about the upload.
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
  # Every checksum the repository publishes on purpose - the sbt distribution's
  # and the pinned base image's - read from the file that publishes them, so
  # that this allowlist cannot drift from it.
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
import stat
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
# published checksum, or BE a hexadecimal digest that an ADJACENT key names as
# one. A value that merely contains one - `password=<checksum>-suffix` - is
# not the checksum and is not excused.
HEX = re.compile(r"[0-9a-fA-F]{32,}")

# The rules a digest keyword may never excuse, as DATA rather than as a
# comment somebody has to remember. `credential-assignment` and
# `url-embedded-credentials` matched a value that the document itself calls a
# password, a token or a URL's userinfo; a 32-character hexadecimal password
# is still a password, and a digest word elsewhere on the line says nothing
# about it. This is the hole the previous version had:
# `commit password=<32 hex>` satisfied both halves of the old test and passed.
CREDENTIAL_RULES = frozenset({"credential-assignment", "url-embedded-credentials"})

# The keys that may name a digest. `commit` and `hash` are deliberately NOT
# here: neither ever governs a value as a key in these artifacts, and both
# appear in prose - "the commit", "hash of" - which is exactly how a word
# anywhere on the line came to excuse a credential. `CODEC-AUDIT-DIGEST` is
# the label the Rule 6 audit prints before its own digest, and it ends in
# `digest`, so the same test covers it.
DIGEST_KEY_WORDS = ("sha1", "sha256", "sha512", "md5", "digest", "checksum")
DIGEST_KEY = re.compile(r"(?i)(?:" + r"|".join(DIGEST_KEY_WORDS) + r")$")

# What may sit between that key and the value it governs: the separators of
# the documents this scans (JSON, YAML, the evidence files' `key: value` and
# `key=value` lines), and no more than a few of them. A longer run means the
# key is not governing this value, it is merely earlier on the line.
DIGEST_SEPARATORS = ":= \t\"'-"
DIGEST_SEPARATOR_LIMIT = 8


def digest_key_governs(line, start):
    """Whether a digest key is the key IMMEDIATELY before the matched value.

    `start` is the offset of the value inside the line. Walking left over the
    separator characters must reach the end of a digest key within
    DIGEST_SEPARATOR_LIMIT characters: `sha256=<hex>`, `"checksum": "<hex>"`
    and `CODEC-AUDIT-DIGEST <hex>` all qualify, while `commit <hex>` does not
    (not a key), and neither does a line that happens to mention a digest
    somewhere else before naming something entirely different.
    """
    prefix = line[:start]
    index = len(prefix)
    while index > 0 and prefix[index - 1] in DIGEST_SEPARATORS:
        index -= 1
    if len(prefix) - index > DIGEST_SEPARATOR_LIMIT:
        return False
    return DIGEST_KEY.search(prefix[:index]) is not None


def digest_allowlist_reason(name, line, match, value):
    """Why this match is a published digest rather than a credential, or None.

    One place decides it, so the rule exclusion and the adjacency requirement
    cannot drift apart from each other or from what the summary reports.
    """
    if name in CREDENTIAL_RULES:
        return None
    if not HEX.fullmatch(value):
        return None
    if not digest_key_governs(line, match.start()):
        return None
    return ("a hexadecimal digest that the key immediately before it names as "
            "one")

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
    # O_NOFOLLOW, then the properties read from the DESCRIPTOR: this decides
    # whether bytes are publishable, so what is examined must be the file the
    # walk found and not whatever its name points at by the time it is opened
    # (CWE-59, CWE-367). A link is refused by the open rather than followed,
    # and a directory, FIFO or device at that name cannot be read as a file.
    try:
        handle = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    except OSError as error:
        errors.append(f"{path}: {error}")
        return
    try:
        if not stat.S_ISREG(os.fstat(handle).st_mode):
            os.close(handle)
            errors.append(f"{path}: not a regular file, so it was not scanned")
            return
        with os.fdopen(handle, "rb") as reader:
            payload = reader.read()
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
                reason = digest_allowlist_reason(name, line, match, value)
                if reason is not None:
                    allowed.append(f"{path}:{number} rule={name} reason={reason}")
                    continue
                findings.append((path, f"{path}:{number} rule={name} {describe(match)}"))


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
    # Stated in the evidence rather than left to be read out of this script,
    # because the scope of an allowlist is the only thing that decides what a
    # clean scan actually proves.
    f"digest allowlist scope   : never applied to these rules - "
    f"{', '.join(sorted(CREDENTIAL_RULES))}; elsewhere it requires one of the "
    f"keys {', '.join(DIGEST_KEY_WORDS)} to end within {DIGEST_SEPARATOR_LIMIT} "
    f"separator characters of the matched value",
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
for _, entry in findings:
    report.append(f"  FINDING {entry}")

def write_record(path, payload):
    """Writes one record of the scan no-follow, refusing a link at its name."""
    try:
        handle = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC | os.O_NOFOLLOW,
                         0o600)
    except OSError as error:
        print(f"the scan record {path} could not be written: {error}")
        sys.exit(1)
    with os.fdopen(handle, "wb") as sink:
        sink.write(payload)


write_record(summary_path, ("\n".join(report) + "\n").encode("utf-8", "surrogateescape"))

# The same findings as a machine-readable list of PATHS beside the summary.
# The publication step acts on a finding - it withdraws that file from the
# published tree and quarantines the original - so the path it acts on has to
# be the path that was scanned, exactly.
#
# Two things are therefore deliberate. The paths are the ones RECORDED with
# each finding rather than re-derived by splitting the formatted line on its
# first colon: a colon is legal in a filename on every filesystem this runs
# on, and that split would truncate such a name at it and leave the file that
# actually carries the credential in the published tree (CWE-116 improper
# encoding leading to CWE-200). And the list is NUL-delimited, because a
# newline or a tab is legal in a filename too and a line-oriented list cannot
# represent one without corrupting it. The values stay withheld here as well:
# a path and a rule name are what the next step needs.
offenders = sorted({path for path, _ in findings})
write_record(summary_path + ".paths",
             "".join(path + "\0" for path in offenders).encode("utf-8", "surrogateescape"))

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

# The paths no part of this delivery may change, and the git object id each
# one had when this baseline was taken. A tree id is the content of the whole
# subtree beneath it and a blob id the content of the file, so an id that
# still matches is proof that the path is byte-for-byte what it was -
# independently of any ref, remote or working-tree state.
PROTECTED_PATHS=(modules examples eclipse pom.xml src .github)
PROTECTED_PATH_IDS=(
  "modules:47c4d8d56e2f0a488a9ca8831a39b2e1251378b9"
  "examples:1acf8f793ed77c8688cd9b633479d49b2b096e22"
  "eclipse:337e2905f43bf42f5c319e5770d1f78b90f3d27e"
  "pom.xml:aabf6fe7b889cb6f7a7d850185841b13dae54a96"
  "src:38d4031ff7b35781988766cf44ff03bfdeb88657"
  ".github:e168f4c1e3862b1bd66a90e92060e336ddea9362"
)
# Tried in order; the first that resolves is the baseline whose ancestry and
# diff are checked.
PROTECTED_BASELINE_REFS=(origin/main origin/master main master)

#=============================================================================
# Publication.
#
# Uploading an artifact is a disclosure, and it is the LAST thing this run
# does, so it is the one step where a mistake cannot be corrected afterwards.
# Three separate problems meet here, and one mechanism answers all three:
#
#   1. A credential signature detected in an artifact used to change nothing
#      but this process's exit status. The file stayed exactly where it was,
#      and `.circleci/config.yml` uploaded that path with `when: always` -
#      which runs precisely when something has failed. Detection has to ACT
#      on the artifact (CWE-200, CWE-532, CWE-693).
#   2. The raw artifacts carry the build machine's internals: every JUnit XML
#      file names the runner's hostname and, in its `<properties>` block,
#      `user.dir`, `user.home`, `java.io.tmpdir` and the JDK's library path;
#      the sbt and class-load logs carry absolute checkout, cache and
#      toolchain paths and the commands that produced them (CWE-200).
#   3. An artifact an earlier run left behind is indistinguishable from one
#      this run produced, so stale evidence can be published as current
#      (CWE-345).
#
# The mechanism: nothing is uploaded from where it was written. Every
# artifact is copied into `target/publish` - a tree this run creates fresh
# and owns, 0700 - and on the way it is proved to be a regular, singly-linked
# file this run wrote, proved to be newer than the run's own start, and
# sanitized. The copies are then SCANNED, and the tree is published only if
# that scan is clean: an artifact that matches is withdrawn from the tree,
# a marker is left in its place, and the ORIGINAL is moved into
# `target/quarantine/<run-id>/`, which nothing uploads.
#
# `target/publish/PUBLICATION-STATUS.txt` is the marker CI and a reader go
# by. It says APPROVED or NOT APPROVED, with the counts and the run identity,
# and it is written on every exit path - a normal finish, a failed row, a
# preflight or pre-run abort, and an interruption - because a run that ends
# early has produced just as much to upload.
#=============================================================================

# The publication is attempted exactly once, and both `main` and the EXIT
# trap may be the one that attempts it.
PUBLICATION_DONE="no"
# Whether the first pass has run, and whether it succeeded. Two variables and
# not one, because "has not run yet" and "ran and failed" lead to different
# things: the first is staged now, the second is already a recorded row.
PUBLICATION_TREES_STAGED="no"
PUBLICATION_TREES_OK="no"
# The list of quarantined originals, for the status file.
PUBLICATION_QUARANTINE_LIST=""
# "yes" only when the tree was staged whole, nothing was withheld and the
# scan of the staged copies came back clean. Read by `on_exit`, which turns a
# publication that was not approved into a non-zero exit status even if every
# gate row passed: the artifacts are the deliverable, and a run that cannot
# publish them safely has not delivered them.
PUBLICATION_APPROVED="no"
# What to say about it, in the status file and in the row detail.
PUBLICATION_DETAIL=""
PUBLICATION_STAGED_COUNT=0
PUBLICATION_WITHHELD_COUNT=0
PUBLICATION_FINDING_COUNT=0
PUBLICATION_QUARANTINED_COUNT=0
PUBLICATION_REDACTION_FILE=""

# publication_redactions - writes the table of paths the published copies
# must not carry, and returns its path in PUBLICATION_REDACTION_FILE.
#
# The table is DATA in the audit tree rather than a list inside this
# function, so that what was redacted from a published artifact can be read
# afterwards from the evidence of the run that redacted it.
#
# Only values that are certainly paths or machine names are listed. The
# account name on its own is deliberately NOT: it is a short word - `root`,
# `circleci` - that occurs inside ordinary English and inside identifiers,
# and replacing it everywhere would corrupt the evidence while the places it
# actually leaks from (the home directory prefix, and the JUnit `<properties>`
# block) are already covered.
publication_redactions() {
  PUBLICATION_REDACTION_FILE="$AUDIT_DIR/publication-redactions.txt"
  safe_truncate "$PUBLICATION_REDACTION_FILE" || return 1

  local -a needles=()
  # Longest first is enforced by the reader, so the order here is only for a
  # human: most specific concern first.
  needles+=("$ROOT	[redacted:checkout]")
  if [[ -n "$ROOT_REAL" && "$ROOT_REAL" != "$ROOT" ]]; then
    needles+=("$ROOT_REAL	[redacted:checkout]")
  fi
  if [[ -n "${HOME:-}" && "$HOME" != "/" ]]; then
    # Covers the coursier, ivy, sbt and maven caches and the account name
    # that the home directory of a named user contains.
    needles+=("$HOME	[redacted:home]")
  fi
  if [[ -n "${JAVA_HOME:-}" && "$JAVA_HOME" != "/" ]]; then
    needles+=("$JAVA_HOME	[redacted:jdk]")
  fi
  # The directories the tools of this run actually came from, resolved rather
  # than guessed: an evidence file names them wherever it records a command,
  # a version or the class it disassembled, and where a toolchain lives is a
  # fact about the build machine and not about the delivery. `type -P` is a
  # builtin lookup, so this needs nothing external, and it deliberately
  # ignores the checked-command FUNCTIONS of the same names.
  local tool resolved directory
  local -a tool_dirs=()
  for tool in git sbt java javap python3 awk sed grep find date; do
    resolved="$(type -P "$tool" 2>/dev/null || printf '')"
    [[ -n "$resolved" ]] || continue
    directory="${resolved%/*}"
    [[ -n "$directory" && "$directory" != "/" ]] || continue
    case " ${tool_dirs[*]-} " in
      *" $directory "*) continue ;;
    esac
    tool_dirs+=("$directory")
  done
  # The conventional toolchain roots as well, because a JDK or an sbt
  # distribution is named in a log by its own layout and not only through the
  # executable that was invoked.
  for tool in /opt/toolchain /usr/lib/jvm /usr/local/sbt /opt/java; do
    case " ${tool_dirs[*]-} " in
      *" $tool "*) continue ;;
    esac
    tool_dirs+=("$tool")
  done
  for tool in "${tool_dirs[@]}"; do
    needles+=("$tool	[redacted:tool]")
  done
  if [[ -n "${HOSTNAME:-}" && "${#HOSTNAME}" -ge 4 ]]; then
    needles+=("$HOSTNAME	[redacted:host]")
  fi
  local temp="${TMPDIR:-/tmp}"
  temp="${temp%/}"
  if [[ -n "$temp" && "$temp" != "/" ]]; then
    needles+=("$temp/	[redacted:tmp]/")
  fi

  if ! {
    printf '# needle<TAB>replacement; applied longest-needle-first to every published copy.\n'
    printf '%s\n' "${needles[@]}"
  } | guarded_write "$PUBLICATION_REDACTION_FILE"; then
    framework_error "the publication redaction table could not be written"
    return 1
  fi
  return 0
}

# publication_status_line <verdict> - the one-line verdict CI greps for.
publication_status_line() {
  printf 'PUBLICATION: %s\n' "$1"
}

# write_publication_status - the marker inside the published tree.
#
# It carries no absolute path and no host detail of its own: repository-
# relative paths, the run identity, and counts. It is the last thing written
# into the tree, and it is rewritten if the final verification scan changes
# the verdict.
write_publication_status() {
  local verdict="$1"
  local quarantine_list="$2"

  safe_truncate "$PUBLICATION_STATUS_FILE" || return 1
  if ! {
    printf '# Publication status of one acceptance-gate run.\n'
    printf '#\n'
    printf '# Everything in this directory is a SANITIZED copy of an artifact under\n'
    printf '# target/, checked to belong to this run and scanned for credential\n'
    printf '# signatures before it was kept. The raw artifacts are not published.\n'
    printf '#\n'
    publication_status_line "$verdict"
    printf 'run-id\t%s\n' "${RUN_ID:-unknown}"
    printf 'started-utc\t%s\n' "${RUN_STARTED_UTC:-unknown}"
    printf 'commit\t%s\n' "${HEAD_COMMIT:-unknown}"
    printf 'branch\t%s\n' "${HEAD_BRANCH:-unknown}"
    printf 'working-tree\t%s\n' "$(markdown_cell "${HEAD_STATE:-unknown}")"
    printf 'gates-failed\t%s\n' "${GATE_FAILED:-unknown}"
    printf 'run-completed\t%s\n' "${RUN_COMPLETED:-no}"
    printf 'staged-artifacts\t%s\n' "$PUBLICATION_STAGED_COUNT"
    printf 'withheld-artifacts\t%s\n' "$PUBLICATION_WITHHELD_COUNT"
    printf 'credential-findings\t%s\n' "$PUBLICATION_FINDING_COUNT"
    printf 'quarantined-originals\t%s\n' "$PUBLICATION_QUARANTINED_COUNT"
    # What the transaction had to do to get here, so a reader can tell a
    # first-pass publication from one that needed the report recomposed after
    # a finding was acted on.
    printf 'passes\t%s\n' "$PUBLICATION_ATTEMPTS"
    printf 'evidence-trees-scanned-clean\t%s\n' "$PUBLICATION_TREES_OK"
    printf 'report-published\t%s\n' "$REPORT_PUBLISHED"
    printf 'incident\t%s\n' "$PUBLICATION_INCIDENT"
    printf 'detail\t%s\n' "$(markdown_cell "${PUBLICATION_DETAIL:-none}")"
    if [[ -n "$quarantine_list" ]]; then
      printf '# originals moved out of every published path (values withheld):\n'
      printf '%s\n' "$quarantine_list"
    fi
    printf '# manifest: MANIFEST.txt lists every staged copy, its source, the sha256\n'
    printf '# of the bytes that were published and what was removed from them; a line\n'
    printf '# whose first field is "-" was NOT published. The uploader re-checks this\n'
    printf '# tree against those digests - `scripts/verify-gates.sh --verify-publication`\n'
    printf '# - so a file changed or added between the scan and the upload is caught.\n'
  } | guarded_write "$PUBLICATION_STATUS_FILE"; then
    framework_error "the publication status file could not be written"
    return 1
  fi
  return 0
}

# read_scan_offenders <scan summary> - the paths the scan recorded, verbatim.
#
# Fills SCAN_OFFENDERS with the NUL-delimited list the scanner writes beside
# its summary. NUL is the one delimiter a filename cannot contain, so a name
# holding a space, a tab, a newline or a colon arrives here exactly as it was
# scanned - and the path that is acted on is therefore the path that carried
# the finding. `read -d ''` is what reads such a list; a `while read` over
# lines would split one name into two and act on neither (CWE-116).
SCAN_OFFENDERS=()
read_scan_offenders() {
  local summary="$1"
  local paths_file="$summary.paths"
  local offender

  SCAN_OFFENDERS=()
  if [[ ! -f "$paths_file" ]]; then
    return 1
  fi
  while IFS= read -r -d '' offender || [[ -n "$offender" ]]; do
    [[ -n "$offender" ]] || continue
    SCAN_OFFENDERS+=("$offender")
  done <"$paths_file"
  return 0
}

# quarantine_scan_offenders <scan summary> <what the artifacts are>
#
# Moves every artifact the scan named out of every path this run or CI can
# publish, into this run's quarantine directory. Detection has to ACT on the
# artifact rather than merely on this process's status: a credential left
# where it was found is still there for the next run, the next step or an
# operator to upload (CWE-200). The count lands in QUARANTINED_COUNT - by
# assignment in the caller's shell, never through a command substitution,
# which would discard it with the subshell.
QUARANTINED_COUNT=0
quarantine_scan_offenders() {
  local summary="$1"
  local what="$2"
  local moved

  QUARANTINED_COUNT=0
  if ! read_scan_offenders "$summary"; then
    framework_error "the scan of $what reported findings but wrote no path list, so nothing could be quarantined"
    return 1
  fi
  if [[ "${#SCAN_OFFENDERS[@]}" -eq 0 ]]; then
    return 0
  fi
  if ! moved="$(fs_guard quarantine "$ROOT" "$QUARANTINE_DIR/${RUN_ID:-unknown-run}" \
    "${SCAN_OFFENDERS[@]}" 2>&1)"; then
    framework_error "$what could not be quarantined: ${moved:-no reason given}"
    return 1
  fi
  QUARANTINED_COUNT="${#SCAN_OFFENDERS[@]}"
  printf 'quarantined %s artifact(s) of %s into %s (%s)\n' \
    "$QUARANTINED_COUNT" "$what" "${QUARANTINE_DIR#"$ROOT"/}/${RUN_ID:-unknown-run}" \
    "$moved" >&2
  return 0
}

# quarantine_publication_findings <scan summary> - acts on what the scan found.
#
# For every staged copy the scan reported: the copy is withdrawn and replaced
# by a marker, and the ORIGINAL it was made from - looked up in the manifest,
# not re-derived from the path - is moved into this run's quarantine
# directory. The list of quarantined originals goes into
# PUBLICATION_QUARANTINE_LIST and the count into
# PUBLICATION_QUARANTINED_COUNT, and it does NOT print them for a caller to
# capture: a command substitution runs in a subshell, where those two
# assignments would be discarded the moment it ended - which is exactly how
# the status file came to report "quarantined-originals 0" beside a
# quarantine directory holding the file.
quarantine_publication_findings() {
  local summary="$1"
  local staged original
  local -a originals=()
  local -a listed=()

  if ! read_scan_offenders "$summary"; then
    framework_error "the publication scan reported findings but wrote no path list"
    return 1
  fi
  for staged in "${SCAN_OFFENDERS[@]}"; do
    [[ -n "$staged" ]] || continue
    # The source is looked up by an exact field match on the staged path, so a
    # name containing a colon, a space or a tab resolves to its own row and to
    # no other. The manifest is read through the same descriptor-checked tree
    # it was written into.
    original="$(command awk -F'\t' -v staged="${staged#target/publish/}" \
      '$1 == staged { print $2; exit }' "$PUBLICATION_MANIFEST_FILE")"
    # The staged copy goes whatever happens: an artifact the scan matched is
    # not published even if its source cannot be identified.
    if ! fs_guard withdraw "$ROOT" "$ROOT/$staged" \
      "a credential signature was found in this artifact" >/dev/null 2>&1; then
      framework_error "the staged copy $staged could not be withdrawn from the publication tree"
      return 1
    fi
    if [[ -n "$original" ]]; then
      originals+=("$original")
      listed+=("$(printf 'quarantined\t%s\t(was staged as %s)' "$original" "$staged")")
    else
      listed+=("$(printf 'withdrawn\t%s\t(no source recorded in the manifest)' "$staged")")
    fi
  done

  if [[ "${#originals[@]}" -gt 0 ]]; then
    local moved
    if ! moved="$(fs_guard quarantine "$ROOT" "$QUARANTINE_DIR/${RUN_ID:-unknown-run}" \
      "${originals[@]}" 2>&1)"; then
      framework_error "an artifact carrying a credential signature could not be quarantined: $moved"
      return 1
    fi
    PUBLICATION_QUARANTINED_COUNT="$(printf '%s\n' "$moved" |
      command sed -n 's/^quarantined=\([0-9]*\)$/\1/p')"
    PUBLICATION_QUARANTINED_COUNT="${PUBLICATION_QUARANTINED_COUNT:-0}"
  fi
  if [[ "${#listed[@]}" -gt 0 ]]; then
    PUBLICATION_QUARANTINE_LIST="$(printf '%s\n' "${listed[@]}")"
  fi
  return 0
}

# stage_publication_trees - the first publication pass: the three directories.
#
# Run BEFORE the report is composed, so that its verdict is a row of the
# report rather than a fact discovered after the report said everything
# passed. The report itself is staged by `publish_gate_report_copy`, which
# cannot run any earlier because the file does not exist yet.
stage_publication_trees() {
  PUBLICATION_TREES_STAGED="yes"
  PUBLICATION_DETAIL=""

  # The tree being published has to be the tree that was validated: a parent
  # renamed mid-run would otherwise redirect the last step of the run.
  verify_output_tree_identity || {
    PUBLICATION_DETAIL="the output tree is no longer the one this run validated"
    return 1
  }

  # Fresh, and owned by this run: whatever is in target/publish arrived from
  # a previous run or from outside, and none of it has been scanned by this
  # one.
  if [[ -e "$PUBLISH_DIR" ]]; then
    local swept
    if ! swept="$(fs_guard rmtree "$ROOT" "$PUBLISH_DIR" 2>&1)"; then
      PUBLICATION_DETAIL="the previous publication tree could not be removed (${swept:-no reason given})"
      framework_error "$PUBLICATION_DETAIL"
      return 1
    fi
  fi
  ensure_output_dir "$PUBLISH_DIR" 0700 || {
    PUBLICATION_DETAIL="the publication tree could not be created"
    return 1
  }
  publication_redactions || {
    PUBLICATION_DETAIL="the redaction table could not be written, so nothing was staged"
    return 1
  }
  safe_truncate "$PUBLICATION_MANIFEST_FILE" || {
    PUBLICATION_DETAIL="the publication manifest could not be started"
    return 1
  }

  local staged_counts staged_rc=0
  staged_counts="$(fs_guard stage "$ROOT" "$PUBLISH_DIR" "$RUN_STARTED_EPOCH" \
    "$PUBLICATION_REDACTION_FILE" "$PUBLICATION_MANIFEST_FILE" truncate \
    "target/parity-report=parity-report" \
    "target/test-reports=test-reports" \
    "target/audit=audit" \
    "target/audit/run-identity.txt=run-identity.txt" 2>&1)" || staged_rc=$?

  PUBLICATION_STAGED_COUNT="$(printf '%s\n' "$staged_counts" |
    command sed -n 's/.*staged=\([0-9]*\) .*/\1/p' | command head -n 1)"
  PUBLICATION_STAGED_COUNT="${PUBLICATION_STAGED_COUNT:-0}"
  local withheld
  withheld="$(printf '%s\n' "$staged_counts" | command awk '
    { for (i = 1; i <= NF; i++) { split($i, kv, "=");
        if (kv[1] == "stale" || kv[1] == "unsafe" || kv[1] == "missing") n += kv[2] } }
    END { print n + 0 }')"
  PUBLICATION_WITHHELD_COUNT="${withheld:-0}"

  # The counts go into the report as an appendix, through the one helper that
  # records its own failure if the appendix cannot be written.
  printf '%s\n' "$staged_counts" |
    add_appendix "Publication - staging of the evidence trees (sanitized copies)"

  # A withheld artifact is a failure of the publication, but it is NOT a
  # reason to skip what follows: the scan runs over whatever WAS staged,
  # always. Returning here - as an earlier version of this function did -
  # meant that one stale file left the copies of every other artifact
  # unscanned, which is the opposite of fail-closed.
  local withheld_detail=""
  if [[ "$staged_rc" -ne 0 ]]; then
    withheld_detail="$PUBLICATION_WITHHELD_COUNT artifact(s) withheld from publication as stale, unsafe or missing (see MANIFEST.txt); $staged_counts"
  fi

  # The scan of the copies: the exact bytes that will be uploaded.
  local summary="$AUDIT_DIR/publication-secret-scan-staged.txt"
  local scan_rc=0
  scan_publication_artifacts staged-publication require-paths require-files "$summary" \
    "${PUBLISH_DIR#"$ROOT"/}" >/dev/null 2>&1 || scan_rc=$?
  if [[ -f "$summary" ]]; then
    PUBLICATION_FINDING_COUNT="$(command awk -F': *' '/^findings /{ print $2; exit }' "$summary")"
    PUBLICATION_FINDING_COUNT="${PUBLICATION_FINDING_COUNT:-0}"
    # A bare call: `add_appendix` records its own failure in the ledger, which
    # is blocking for the run, and `errexit` is suppressed in every context
    # this function is called from.
    add_appendix "Publication - secret scan of the staged copies" <"$summary"
  else
    PUBLICATION_DETAIL="${withheld_detail:+$withheld_detail; }the scan of the staged copies wrote no summary"
    return 1
  fi
  if [[ "$scan_rc" -eq 0 ]]; then
    PUBLICATION_TREES_OK="yes"
    if [[ -n "$withheld_detail" ]]; then
      PUBLICATION_INCIDENT="yes"
      PUBLICATION_DETAIL="$withheld_detail; the $PUBLICATION_STAGED_COUNT artifact(s) that WERE staged scanned clean"
      return 0
    fi
    PUBLICATION_DETAIL="$PUBLICATION_STAGED_COUNT sanitized artifact(s) staged and scanned clean"
    return 0
  fi

  # A finding, or an unreadable file, or a symlink inside the staged tree:
  # the artifact is withdrawn and quarantined, and the tree that remains is
  # re-scanned so that what is published is proved clean rather than assumed
  # to be.
  if [[ "$PUBLICATION_FINDING_COUNT" -gt 0 ]]; then
    quarantine_publication_findings "$summary" || return 1
    local recheck_rc=0
    scan_publication_artifacts staged-publication-recheck require-paths require-files \
      "$AUDIT_DIR/publication-secret-scan-staged-recheck.txt" "${PUBLISH_DIR#"$ROOT"/}" \
      >/dev/null 2>&1 || recheck_rc=$?
    if [[ "$recheck_rc" -ne 0 ]]; then
      PUBLICATION_DETAIL="${withheld_detail:+$withheld_detail; }$PUBLICATION_FINDING_COUNT credential signature(s) found in the staged copies; the offending artifacts were withdrawn and quarantined, but the remaining tree STILL does not scan clean"
      return 1
    fi
    # Acted on, and the tree that remains is PROVED clean: it may be
    # published, and the finding is carried by a blocking row of the report
    # and by the incident lines of the status marker.
    PUBLICATION_TREES_OK="yes"
    PUBLICATION_INCIDENT="yes"
    PUBLICATION_DETAIL="${withheld_detail:+$withheld_detail; }$PUBLICATION_FINDING_COUNT credential signature(s) found in the staged copies; those artifacts were withdrawn from the publication tree and their $PUBLICATION_QUARANTINED_COUNT original(s) quarantined under ${QUARANTINE_DIR#"$ROOT"/}/${RUN_ID:-unknown-run}"
    return 0
  fi
  PUBLICATION_DETAIL="${withheld_detail:+$withheld_detail; }the scan of the staged copies did not pass (see ${summary#"$ROOT"/})"
  return 1
}

# publish_gate_report_copy - the second pass: the report itself.
#
# The report is the one publication artifact that does not exist while the
# rows are running, so it is staged last. Its bytes have already been scanned
# once, before they were renamed into place (see `write_report`); this scans
# the file that is actually on disk and then the sanitized copy of it, so the
# thing uploaded is the thing scanned.
publish_gate_report_copy() {
  if [[ ! -f "$REPORT_FILE" ]]; then
    PUBLICATION_DETAIL="${PUBLICATION_DETAIL:+$PUBLICATION_DETAIL; }no gate report was written, so none is published"
    return 1
  fi
  local summary="$AUDIT_DIR/publication-secret-scan-report.txt"
  local scan_rc=0
  scan_publication_artifacts composed-report require-paths require-files "$summary" \
    "${REPORT_FILE#"$ROOT"/}" >/dev/null 2>&1 || scan_rc=$?
  if [[ "$scan_rc" -ne 0 ]]; then
    PUBLICATION_DETAIL="${PUBLICATION_DETAIL:+$PUBLICATION_DETAIL; }the composed gate report did not pass the secret scan, so it is NOT published (see ${summary#"$ROOT"/})"
    return 1
  fi
  local staged_counts staged_rc=0
  staged_counts="$(fs_guard stage "$ROOT" "$PUBLISH_DIR" "$RUN_STARTED_EPOCH" \
    "$PUBLICATION_REDACTION_FILE" "$PUBLICATION_MANIFEST_FILE" append \
    "target/gate-report.md=gate-report.md" 2>&1)" || staged_rc=$?
  if [[ "$staged_rc" -ne 0 ]]; then
    PUBLICATION_DETAIL="${PUBLICATION_DETAIL:+$PUBLICATION_DETAIL; }the gate report could not be staged for publication ($staged_counts)"
    return 1
  fi
  PUBLICATION_STAGED_COUNT=$((PUBLICATION_STAGED_COUNT + 1))
  return 0
}

# publish_report_and_verify - stages the report copy and verifies the tree.
#
# Returns 0 when everything that would be uploaded has been scanned clean, 1
# when a credential signature was FOUND (which is actionable: the offending
# copy can be withdrawn and its original quarantined), and 2 when the tree
# cannot be proved clean at all - an unreadable file, a symlink, a scanner
# that did not run, or a report that is not there. The distinction is what
# decides between cleaning the tree and replacing it: a finding names the
# artifact to act on, while a gap in the scan names nothing and leaves no
# basis for publishing any of it.
publish_report_and_verify() {
  publish_gate_report_copy || return 2

  local summary="$AUDIT_DIR/publication-secret-scan-final.txt"
  local rc=0
  scan_publication_artifacts publication-final require-paths require-files \
    "$summary" "${PUBLISH_DIR#"$ROOT"/}" >/dev/null 2>&1 || rc=$?
  if [[ "$rc" -eq 0 ]]; then
    return 0
  fi
  local found
  found="$(scan_findings_count "$summary")"
  if [[ "$found" -gt 0 ]]; then
    PUBLICATION_FINDING_COUNT=$((PUBLICATION_FINDING_COUNT + found))
    PUBLICATION_DETAIL="${PUBLICATION_DETAIL:+$PUBLICATION_DETAIL; }$found credential signature(s) found in the final scan of the publication tree"
    return 1
  fi
  PUBLICATION_DETAIL="${PUBLICATION_DETAIL:+$PUBLICATION_DETAIL; }the final scan of the publication tree could not be completed (see ${summary#"$ROOT"/})"
  return 2
}

# scan_findings_count <summary> - how many findings that scan recorded.
scan_findings_count() {
  local summary="$1"
  local count=""
  if [[ -f "$summary" ]]; then
    count="$(command awk -F': *' '/^findings /{ print $2; exit }' "$summary")"
  fi
  printf '%s\n' "${count:-0}"
}

# build_incident_tree <reason> - the upload root, replaced by a notice.
#
# Reached when the publication tree cannot be proved clean. The tree is
# REMOVED and a new one created in its place, holding nothing but text this
# script generated: an unverifiable tree must not be uploaded, and leaving it
# where CI collects from - which is what merely returning a non-zero status
# did - uploads it anyway (CWE-200). The evidence itself stays on the machine,
# under target/audit and target/quarantine, for an operator with access to it.
build_incident_tree() {
  local reason="$1"
  local swept

  if [[ -e "$PUBLISH_DIR" ]]; then
    if ! swept="$(fs_guard rmtree "$ROOT" "$PUBLISH_DIR" 2>&1)"; then
      printf 'FATAL: the publication tree could not be replaced (%s), and it has NOT\n' \
        "${swept:-no reason given}" >&2
      printf 'been proved safe to upload. Remove %s by hand before publishing.\n' \
        "${PUBLISH_DIR#"$ROOT"/}" >&2
      return 1
    fi
  fi
  ensure_output_dir "$PUBLISH_DIR" 0700 || return 1
  # `store_test_results` in CI points at this directory, so it exists even
  # here; empty is a result CI can collect, a missing path is an error.
  ensure_output_dir "$PUBLISH_DIR/test-reports" 0700 || return 1

  if ! {
    printf 'This directory deliberately holds no evidence.\n\n'
    printf 'The acceptance run could not prove that the artifacts it produced were\n'
    printf 'safe to publish, so the publication tree was replaced by this notice\n'
    printf 'rather than uploaded. Nothing here was withheld to hide a result: the\n'
    printf 'evidence exists on the machine that ran the gates, under target/audit,\n'
    printf 'target/parity-report and target/test-reports, and anything that matched\n'
    printf 'a credential signature is under target/quarantine.\n\n'
    printf 'reason: %s\n' "$(markdown_cell "$reason")"
    printf 'run-id: %s\n' "$(markdown_cell "${RUN_ID:-unknown}")"
    printf 'commit: %s\n' "$(markdown_cell "${HEAD_COMMIT:-unknown}")"
    printf 'started-utc: %s\n' "$(markdown_cell "${RUN_STARTED_UTC:-unknown}")"
  } | guarded_write "$PUBLISH_DIR/NOTICE.txt"; then
    framework_error "the publication incident notice could not be written"
    return 1
  fi
  if ! {
    printf '# staged-path\tsource-path\tpublished-bytes\tsha256\tnotes\n'
    printf -- '-\t(nothing)\t0\t-\tNOT PUBLISHED: the publication tree could not be proved clean\n'
  } | guarded_write "$PUBLICATION_MANIFEST_FILE"; then
    framework_error "the incident manifest could not be written"
    return 1
  fi
  PUBLICATION_STAGED_COUNT=0
  write_publication_status "NOT APPROVED - $reason" "$PUBLICATION_QUARANTINE_LIST" || return 1

  # Even a notice is scanned before it is left where CI collects from. If it
  # cannot be scanned clean, there is nothing left to publish at all.
  local rc=0
  scan_publication_artifacts publication-incident require-paths require-files \
    "$AUDIT_DIR/publication-secret-scan-incident.txt" "${PUBLISH_DIR#"$ROOT"/}" \
    >/dev/null 2>&1 || rc=$?
  if [[ "$rc" -ne 0 ]]; then
    printf 'FATAL: even the publication notice did not scan clean; the upload root is\n' >&2
    printf 'being removed entirely.\n' >&2
    if ! fs_guard rmtree "$ROOT" "$PUBLISH_DIR" >/dev/null 2>&1; then
      printf 'WARNING: %s could not be removed either; do not upload it.\n' \
        "${PUBLISH_DIR#"$ROOT"/}" >&2
    fi
    return 1
  fi
  return 0
}

# finalize_publication - composing, staging and verifying as ONE transaction.
#
# The ordering this replaces was the defect: the report was composed, renamed
# into place and marked written, and only AFTERWARDS was the tree that
# contains it scanned - so a failure at that point left an already-staged
# report saying "every automated gate passed", with the failure recorded
# nowhere a reader of the published tree would see it (CWE-345). Composition,
# staging and the final scan are therefore one unit with a single terminal
# state, and nothing is marked published until the scan that governs the
# uploaded bytes has passed.
#
# The transaction runs at most twice. The first pass stages the trees, composes
# the report and verifies. A finding is ACTED on - the offending copy
# withdrawn, its original quarantined, a blocking row recorded - and the
# second pass recomposes the report so that the published report states the
# finding, then verifies again. A gap that cannot be closed, or a second pass
# that still does not verify, replaces the upload root with an incident-only
# tree.
#
# Called by `main` and by the EXIT trap, and idempotent: whichever reaches it
# first finalizes, and the other reads the verdict.
finalize_publication() {
  if [[ "$PUBLICATION_DONE" == "yes" ]]; then
    [[ "$PUBLICATION_APPROVED" == "yes" ]]
    return
  fi

  local attempt outcome report_rc
  for attempt in 1 2; do
    PUBLICATION_ATTEMPTS="$attempt"

    if [[ "$attempt" -eq 1 && "$PUBLICATION_TREES_STAGED" != "yes" ]]; then
      if ! stage_publication_trees; then
        record_blocking_row "Publication - scan-approved staging of the evidence trees" \
          "${PUBLICATION_DETAIL:-the publication staging did not complete}" \
          "target/publish/MANIFEST.txt" \
          blocking
      elif [[ "$PUBLICATION_INCIDENT" == "yes" ]]; then
        record_blocking_row "Publication - an artifact was withheld or quarantined" \
          "${PUBLICATION_DETAIL:-an artifact did not reach the publication tree}" \
          "target/publish/MANIFEST.txt" \
          blocking
      fi
    fi

    report_rc=0
    if [[ "$attempt" -eq 1 ]]; then
      write_report || report_rc=$?
    else
      write_report recompose || report_rc=$?
    fi
    if [[ "$report_rc" -ne 0 ]]; then
      # `write_report` records its own blocking row for the reason it failed,
      # and says it on stderr. Without a report there is nothing to publish.
      PUBLICATION_DETAIL="${PUBLICATION_DETAIL:+$PUBLICATION_DETAIL; }the gate report could not be published"
      break
    fi

    outcome=0
    publish_report_and_verify || outcome=$?
    if [[ "$outcome" -eq 0 ]]; then
      PUBLICATION_DONE="yes"
      PUBLICATION_APPROVED="yes"
      REPORT_PUBLISHED="yes"
      local verdict
      if [[ "$PUBLICATION_INCIDENT" == "yes" ]]; then
        verdict="APPROVED WITH INCIDENT - every published copy was sanitized and scanned clean, and ${PUBLICATION_DETAIL:-an artifact was withheld or quarantined}"
      else
        verdict="APPROVED - every published copy was sanitized and scanned clean"
      fi
      if ! write_publication_status "$verdict" "$PUBLICATION_QUARANTINE_LIST"; then
        PUBLICATION_APPROVED="no"
        if ! build_incident_tree "the publication status marker could not be written"; then
          printf 'WARNING: the upload root could not be replaced by a notice.\n' >&2
        fi
        printf '\nFATAL: publication NOT APPROVED - the status marker could not be written\n' >&2
        return 1
      fi
      printf '\npublication: %s - %s staged artifact(s) in %s\n' \
        "${verdict%% - *}" "$PUBLICATION_STAGED_COUNT" "${PUBLISH_DIR#"$ROOT"/}"
      return 0
    fi

    if [[ "$outcome" -eq 1 && "$attempt" -eq 1 ]]; then
      # Actionable: the scan named the artifacts. They are withdrawn from the
      # tree and their originals quarantined, the finding is recorded as a
      # blocking row, and the second pass recomposes the report so that the
      # report which is published states it.
      quarantine_publication_findings "$AUDIT_DIR/publication-secret-scan-final.txt" ||
        framework_error "the artifacts the final scan named could not all be acted on"
      PUBLICATION_INCIDENT="yes"
      record_blocking_row "Publication - credential signature in the staged evidence" \
        "${PUBLICATION_DETAIL:-a credential signature was found in the publication tree}" \
        "target/audit/publication-secret-scan-final.txt" \
        blocking
      continue
    fi
    break
  done

  # Neither pass produced a tree that could be proved clean.
  PUBLICATION_DONE="yes"
  PUBLICATION_APPROVED="no"
  record_blocking_row "Publication - the evidence could not be published safely" \
    "${PUBLICATION_DETAIL:-the publication tree could not be proved clean}" \
    "target/audit/publication-secret-scan-final.txt" \
    blocking
  # The report on disk should state that row too, which is the last thing this
  # writes. It is not published - that is what this branch has established -
  # but it is the local record an operator reads.
  if ! write_report recompose; then
    printf 'WARNING: the local report could not be recomposed with that row.\n' >&2
  fi
  if ! build_incident_tree "${PUBLICATION_DETAIL:-the publication tree could not be proved clean}"; then
    printf 'WARNING: the upload root could not be replaced by a notice.\n' >&2
  fi
  printf '\nFATAL: publication NOT APPROVED - %s\n' \
    "${PUBLICATION_DETAIL:-the publication step did not complete}" >&2
  printf 'The upload root %s has been replaced by a notice, so nothing unverified\n' \
    "${PUBLISH_DIR#"$ROOT"/}" >&2
  printf 'leaves this machine. The evidence itself is under %s and %s.\n' \
    "${AUDIT_DIR#"$ROOT"/}" "${QUARANTINE_DIR#"$ROOT"/}" >&2
  return 1
}

#-----------------------------------------------------------------------------
# Framework self-checks.
#
# Every control below exists because a specific defect was found in this
# framework, and a fix with no control over it is a fix that silently comes
# undone: the next person to touch the scanner's allowlist, the report
# encoders, the ledger helper, the write path or the sanitizer has no way to
# know which behaviour was deliberate. These run on EVERY acceptance run,
# before the first gate row, and they invoke the real mechanisms rather than
# re-implementing them - a control that tests a copy of the logic proves
# nothing about the logic that runs.
#
# They are hermetic: their fixtures live in a private directory under target/
# that is not one of the publication paths, and it is removed as soon as they
# finish, so the credential-shaped strings they need are never staged, never
# scanned as evidence and never in the report. What reaches the evidence is
# the verdict of each control and nothing of its fixture.
#
# A failed control is BLOCKING and stops the run with exit 2: if the framework
# that produces the evidence cannot be shown to behave as specified, the
# evidence it would produce is not worth measuring anything against.
#-----------------------------------------------------------------------------

SELF_CHECK_FAILURES=()
SELF_CHECK_RESULTS=()

# self_check <description> <expected> <actual>
self_check() {
  if [[ "$2" == "$3" ]]; then
    SELF_CHECK_RESULTS+=("$(printf 'ok      %s (%s)' "$1" "$2")")
    return 0
  fi
  SELF_CHECK_RESULTS+=("$(printf 'FAILED  %s: expected [%s], got [%s]' "$1" "$2" "$3")")
  SELF_CHECK_FAILURES+=("$1")
  return 1
}

# The scanner's credential-context rule (the bypass that allowlisted any long
# hex value whenever a digest word appeared anywhere on the line).
self_check_scanner() {
  local work="$1"
  local fixture="$work/scanner-fixture.txt"
  local relative="${fixture#"$ROOT"/}"
  local summary="$work/scanner-scan.txt"
  local hex32="0123456789abcdef0123456789abcdef"
  local hex64="$hex32$hex32"
  local published

  # Three lines that a digest-word allowlist waves through and a credential
  # rule must not: the key is the credential, the digest word is only nearby.
  if ! {
    printf 'commit password=%s\n' "$hex32"
    printf 'sha256 token=%s\n' "$hex64"
    printf 'token: %s  # sha256 of the jar\n' "$hex32"
  } | guarded_write "$fixture"; then
    self_check "the scanner fixture could be written" "yes" "no"
    return 1
  fi
  local rc=0
  scan_publication_artifacts self-check require-paths require-files "$summary" \
    "$relative" >/dev/null 2>&1 || rc=$?
  self_check "the scanner refuses a credential beside a digest word" "1" "$rc"
  self_check "  all three forms are findings" "3" \
    "$(command awk -F': *' '/^findings /{ print $2; exit }' "$summary" 2>/dev/null || printf 'none')"
  self_check "  and none is allowlisted" "0" \
    "$(command awk -F': *' '/^allowlisted matches /{ print $2; exit }' "$summary" 2>/dev/null || printf 'none')"

  # The one allowlist that must still work: the sbt distribution checksum this
  # repository publishes in .circleci/config.yml.
  published="$(command grep -oE '[0-9a-f]{64}' .circleci/config.yml 2>/dev/null |
    command sort -u | command head -n 1 || printf '')"
  if [[ -n "$published" ]]; then
    # In credential-assignment shape, so the rule fires and the ONLY thing
    # that can stop it being a finding is the published-checksum allowlist:
    # the digest-key path is closed to credential rules by design.
    if ! printf 'token=%s\n' "$published" |
      guarded_write "$fixture"; then
      self_check "the checksum fixture could be written" "yes" "no"
      return 1
    fi
    rc=0
    scan_publication_artifacts self-check-allowlist require-paths require-files \
      "$summary" "$relative" >/dev/null 2>&1 || rc=$?
    self_check "the published sbt checksum is still allowlisted" "0" "$rc"
    self_check "  recorded as an allowlisted match" "1" \
      "$(command awk -F': *' '/^allowlisted matches /{ print $2; exit }' "$summary" 2>/dev/null || printf 'none')"
  fi
  return 0
}

# The encoders that stand between a value this script did not choose and a
# document or a command that parses it.
self_check_encoders() {
  self_check "markdown_cell escapes a table pipe" 'a\|b' "$(markdown_cell 'a|b')"
  self_check "markdown_cell neutralises a newline" 'a\nb' "$(markdown_cell "$(printf 'a\nb')")"
  self_check "markdown_cell escapes a backslash" 'a\\b' "$(markdown_cell 'a\b')"
  self_check "scala_string_literal quotes a quote" '"a\"b"' "$(scala_string_literal 'a"b')"
  local rc=0
  scala_string_literal "$(printf 'a\tb')" >/dev/null 2>&1 || rc=$?
  self_check "scala_string_literal refuses a control character" "1" "$rc"
  # Each probe is an ABSOLUTE path, so the character rule is what decides it
  # rather than the absolute-path rule one line above it - a relative probe
  # would be refused for the wrong reason and the control would pass however
  # the character set were spelled.
  local probe
  for probe in '/tmp/name"with-a-quote' '/tmp/name$with-a-dollar' \
    '/tmp/name`with-a-backtick' '/tmp/name\with-a-backslash'; do
    rc=0
    ( assert_root_is_safe "$probe" >/dev/null 2>&1 ) || rc=$?
    self_check "assert_root_is_safe refuses ${probe#/tmp/name}" "1" "$rc"
  done
  # The space is the one character of the awkward set this script SUPPORTS,
  # because every path expansion is quoted; the control states that, so that
  # re-tightening the set silently is not possible.
  rc=0
  ( assert_root_is_safe '/tmp/a checkout with spaces' >/dev/null 2>&1 ) || rc=$?
  self_check "  and accepts a space, which nothing here has to escape" "0" "$rc"
  rc=0
  ( assert_root_is_safe 'relative/path' >/dev/null 2>&1 ) || rc=$?
  self_check "  and refuses a path that is not absolute" "1" "$rc"
  rc=0
  ( assert_root_is_safe "$ROOT" >/dev/null 2>&1 ) || rc=$?
  self_check "  and accepts this checkout" "0" "$rc"
  return 0
}

# The appendix fence, which is what keeps evidence inside the block that
# quotes it. The section it writes is legitimate evidence and stays in the
# report; its content is three harmless lines.
self_check_appendix_fence() {
  local before after
  before="$(command wc -c <"$(guarded_fd_path "$APPENDIX_FD")" 2>/dev/null || printf 0)"
  {
    printf 'a line of evidence\n'
    printf '```\n'
    printf 'SELF-CHECK-PROBE this line follows a three-backtick sequence\n'
  } | add_appendix "Framework self-check - appendix fencing"
  after="$(command sed -n '/^### Framework self-check - appendix fencing$/,$p' \
    "$(guarded_fd_path "$APPENDIX_FD")" 2>/dev/null || printf '')"
  self_check "the appendix fence outgrows its content" "yes" \
    "$(printf '%s' "$after" | command grep -q '^````text$' && printf 'yes' || printf 'no')"
  self_check "  so what follows the backticks stays inside it" "yes" \
    "$(printf '%s' "$after" | command awk '/^````text$/{inside=1;next} /^````$/{inside=0} inside && /^SELF-CHECK-PROBE/{found=1} END{print (found ? "yes" : "no")}')"
  self_check "  and the appendix grew" "yes" \
    "$([[ "$before" -lt "$(command wc -c <"$(guarded_fd_path "$APPENDIX_FD")" 2>/dev/null || printf 0)" ]] && printf 'yes' || printf 'no')"
  return 0
}

# The ledger helper: all six arrays move together, or the report's table and
# its counts describe different runs. Run in a subshell so the rows it records
# are discarded rather than added to this run's table.
self_check_ledger() {
  local measured
  # The counts before the row. A divergence already present is not this
  # control's subject - it is measured as a delta - so the status of this call
  # is reported rather than tolerated.
  if ! gate_counts; then
    self_check "the ledger was consistent before this control ran" "yes" "no"
  fi
  local was_labels="${#GATE_LABEL[@]}"
  local was_statuses="${#GATE_STATUS[@]}"
  local was_details="${#GATE_DETAIL[@]}"
  local was_commands="${#GATE_COMMAND[@]}"
  local was_evidence="${#GATE_EVIDENCE[@]}"
  local was_kinds="${#GATE_KIND[@]}"
  local was_blocking="$GATE_COUNT_BLOCKING_FAILED"
  local was_failed="$GATE_FAILED"
  local was_automated="$GATE_COUNT_AUTOMATED"
  measured="$(
    # The probe row's own console output is discarded. `record_blocking_row`
    # announces the row it records on stderr, which is right for a real row
    # and wrong for a probe: a reader of the run's output would see
    # "FAIL: self-check row" and take it for a gate that failed. The row
    # itself is discarded with the subshell; only the deltas are measured.
    record_blocking_row "self-check row" "detail" "target/audit/self-check.txt" blocking \
      >/dev/null 2>&1
    gate_counts || printf 'diverged '
    printf '%s/%s/%s/%s/%s/%s blocking=%s failed=%s automated=%s' \
      "$((${#GATE_LABEL[@]} - was_labels))" "$((${#GATE_STATUS[@]} - was_statuses))" \
      "$((${#GATE_DETAIL[@]} - was_details))" "$((${#GATE_COMMAND[@]} - was_commands))" \
      "$((${#GATE_EVIDENCE[@]} - was_evidence))" \
      "$((${#GATE_KIND[@]} - was_kinds))" \
      "$((GATE_COUNT_BLOCKING_FAILED - was_blocking))" \
      "$((GATE_FAILED - was_failed))" \
      "$((GATE_COUNT_AUTOMATED - was_automated))"
  )"
  self_check "record_blocking_row moves all six arrays and tallies as blocking" \
    "1/1/1/1/1/1 blocking=1 failed=1 automated=0" "$measured"
  # The Command column of a row recorded outside the table is not left empty:
  # the report states that the row is not one of the table's measurements.
  measured="$(
    record_blocking_row "self-check row" "detail" "target/audit/self-check.txt" blocking \
      >/dev/null 2>&1
    printf '%s' "${GATE_COMMAND[-1]}"
  )"
  self_check "  and says in the Command column that it measured nothing" "yes" \
    "$(case "$measured" in "$GATE_COMMAND_OUTSIDE_TABLE") printf 'yes' ;; *) printf 'no' ;; esac)"
  return 0
}

# Provenance at staging time, and the write path that cannot be redirected.
self_check_publication_safety() {
  local work="$1"
  local tree="$work/evidence"
  local publish="$work/publish"
  local table=""
  local manifest="$work/manifest.txt"

  ensure_output_dir "$tree" 0700 || return 1
  ensure_output_dir "$publish" 0700 || return 1
  # The real table, written by the real function: a control that wrote its own
  # would pass however the placeholders were spelled.
  publication_redactions || return 1
  table="$PUBLICATION_REDACTION_FILE"
  printf 'this run\n' | guarded_write "$tree/fresh.txt" || return 1
  printf 'an earlier run\n' | guarded_write "$tree/previous.txt" || return 1
  command touch -d '2001-01-01 00:00:00' "$tree/previous.txt"
  printf 'tomorrow\n' | guarded_write "$tree/ahead.txt" || return 1
  command touch -d '+1 hour' "$tree/ahead.txt"
  printf '<?xml version="1.0"?>\n<testsuite hostname="h" name="S" tests="1"><properties><property name="user.dir" value="%s"/></properties><testcase name="a"><failure message="at %s/x.scala">%s/x.scala:1</failure></testcase></testsuite>\n' \
    "$ROOT" "$ROOT" "$ROOT" | guarded_write "$tree/TEST-selfcheck.xml" || return 1

  local counts rc=0
  counts="$(fs_guard stage "$ROOT" "$publish" "$RUN_STARTED_EPOCH" "$table" "$manifest" \
    truncate "${tree#"$ROOT"/}=staged" 2>&1)" || rc=$?
  self_check "staging withholds artifacts that are not this run's" "1" "$rc"
  self_check "  two of them, by date" "stale=2" \
    "$(printf '%s' "$counts" | command grep -o 'stale=[0-9]*' || printf 'none')"
  self_check "  and stages the two that are" "staged=2" \
    "$(printf '%s' "$counts" | command grep -o 'staged=[0-9]*' || printf 'none')"
  self_check "  the sanitized XML still parses" "parses" \
    "$(python3 -c 'import sys,xml.etree.ElementTree as E
E.parse(sys.argv[1]); print("parses")' "$publish/staged/TEST-selfcheck.xml" 2>/dev/null || printf 'no')"
  # `grep -c` exits 1 when it counted zero, which is the PASS here, so its
  # status is discarded and the count itself is the answer; an unreadable file
  # yields no count at all and fails the control.
  self_check "  and carries no checkout path" "0" \
    "$(command grep -c -- "$ROOT" "$publish/staged/TEST-selfcheck.xml" 2>/dev/null; true)"

  # The write path: a name planted with a link is replaced, never written
  # through, and an append to a hard-linked file is refused outright.
  printf 'VICTIM\n' | guarded_write "$work/victim.txt" || return 1
  command ln -s "$work/victim.txt" "$tree/via-link.txt"
  printf 'this run\n' | guarded_write "$tree/via-link.txt" >/dev/null 2>&1
  self_check "a planted link is replaced, not written through" "VICTIM" \
    "$(command cat "$work/victim.txt" 2>/dev/null || printf 'gone')"
  command ln "$work/victim.txt" "$tree/hard.txt"
  rc=0
  printf 'x\n' | guarded_append "$tree/hard.txt" >/dev/null 2>&1 || rc=$?
  self_check "an append to a hard-linked file is refused" "1" "$rc"
  self_check "  and its other name is untouched" "VICTIM" \
    "$(command cat "$work/victim.txt" 2>/dev/null || printf 'gone')"

  # The report's publication step. `mv -f` was used for it once, and `mv -f
  # file directory` moves the file INSIDE the directory and exits 0 - so a
  # directory at the report's name left the run printing "report written" for
  # a report that was not at that name at all. A link there is a milder case:
  # `mv` replaces the link rather than writing through it, so nothing outside
  # the tree is harmed, but the run should not quietly consume an operator's
  # link either. The guarded rename refuses both, and replaces an ordinary
  # file, which is the only case that may proceed.
  printf 'assembly\n' | guarded_write "$tree/report.partial" || return 1
  rc=0
  command mkdir -p "$tree/report.md"
  printf 'occupied\n' | guarded_write "$tree/report.md/occupant" || return 1
  fs_guard rename "$ROOT" "$tree/report.partial" "$tree/report.md" \
    >/dev/null 2>&1 || rc=$?
  self_check "a directory at the report's name refuses the rename" "1" "$rc"
  self_check "  and the assembly is still there to keep" "assembly" \
    "$(command cat "$tree/report.partial" 2>/dev/null || printf 'gone')"
  command rm -rf "$tree/report.md"
  command ln -s "$work/victim.txt" "$tree/report.md"
  rc=0
  fs_guard rename "$ROOT" "$tree/report.partial" "$tree/report.md" \
    >/dev/null 2>&1 || rc=$?
  self_check "a link at the report's name refuses the rename" "1" "$rc"
  self_check "  and the file it pointed at is untouched" "VICTIM" \
    "$(command cat "$work/victim.txt" 2>/dev/null || printf 'gone')"
  command rm -f "$tree/report.md"
  rc=0
  fs_guard rename "$ROOT" "$tree/report.partial" "$tree/report.md" \
    >/dev/null 2>&1 || rc=$?
  self_check "an ordinary destination is published" "0" "$rc"
  self_check "  and it carries the assembled bytes" "assembly" \
    "$(command cat "$tree/report.md" 2>/dev/null || printf 'gone')"
  return 0
}

run_framework_self_checks() {
  local work="$TARGET_DIR/.gate-selftest"
  local evidence_file="$AUDIT_DIR/framework-self-checks.txt"
  local guard

  SELF_CHECK_FAILURES=()
  SELF_CHECK_RESULTS=()

  ensure_output_dir "$work" 0700 || return 1
  if ! guard="$(fs_guard prune "$ROOT" "$work" 2>&1)"; then
    path_fatal "the self-check directory could not be emptied (${guard:-no reason given})"
    return 1
  fi

  # Each of these records its own verdicts through `self_check` and returns
  # non-zero only when a FIXTURE could not be created - which is itself
  # recorded as a failed control, so there is nothing here to tolerate and
  # nothing to hide: the failure list below is the verdict.
  if ! self_check_scanner "$work"; then
    self_check "the scanner controls could run" "yes" "no"
  fi
  if ! self_check_encoders; then
    self_check "the encoder controls could run" "yes" "no"
  fi
  if ! self_check_appendix_fence; then
    self_check "the appendix-fence control could run" "yes" "no"
  fi
  if ! self_check_ledger; then
    self_check "the ledger control could run" "yes" "no"
  fi
  if ! self_check_publication_safety "$work"; then
    self_check "the publication-safety controls could run" "yes" "no"
  fi

  # The fixtures go before anything else can read them; only verdicts remain.
  if ! guard="$(fs_guard rmtree "$ROOT" "$work" 2>&1)"; then
    printf 'WARNING: the self-check fixtures under %s could not be removed (%s).\n' \
      "${work#"$ROOT"/}" "${guard:-no reason given}" >&2
  fi

  if ! {
    printf '## framework self-checks\n'
    printf '# Controls over the behaviour of this framework itself, run before the\n'
    printf '# first gate row. Each one invokes the real mechanism; the fixtures they\n'
    printf '# needed have been removed and no fixture content is recorded here.\n'
    printf '#\n'
    printf '%s\n' "${SELF_CHECK_RESULTS[@]}"
    printf '#\n'
    printf 'controls\t%s\n' "${#SELF_CHECK_RESULTS[@]}"
    printf 'failed\t%s\n' "${#SELF_CHECK_FAILURES[@]}"
  } | guarded_write "$evidence_file"; then
    framework_error "the self-check record could not be written"
    return 1
  fi

  if [[ "${#SELF_CHECK_FAILURES[@]}" -gt 0 ]]; then
    printf 'FATAL: %s framework self-check(s) failed:\n' "${#SELF_CHECK_FAILURES[@]}" >&2
    printf '  %s\n' "${SELF_CHECK_FAILURES[@]}" >&2
    printf 'These are controls over the framework that produces the evidence - the\n' >&2
    printf 'secret scanner, the report encoders, the row ledger, the write path and\n' >&2
    printf 'the publication sanitizer. Until they behave as specified, nothing this\n' >&2
    printf 'run measured can be relied on. See %s\n' "${evidence_file#"$ROOT"/}" >&2
    record_blocking_row "Framework self-checks" \
      "${#SELF_CHECK_FAILURES[@]} of ${#SELF_CHECK_RESULTS[@]} control(s) over this framework failed: ${SELF_CHECK_FAILURES[*]}" \
      "${evidence_file#"$ROOT"/}" \
      blocking
    return 1
  fi
  printf 'framework self-checks: %s control(s) passed\n' "${#SELF_CHECK_RESULTS[@]}"
  return 0
}

# verify_publication_tree - the uploader's check, run by whoever uploads.
#
# The scan approves BYTES, and between that approval and the upload the tree
# sits on disk in a directory another process of the same account can still
# write to: the run has released its lock by then, and CI collects the tree by
# pathname afterwards (CWE-345 insufficient verification). So the approval is
# carried across that gap by the manifest - every published copy with the
# sha256 of the bytes that were approved - and this re-checks it.
#
# Three things have to hold, and all three are refusals rather than warnings:
# the status marker says APPROVED, every file the manifest names is present
# and still digests to what was approved, and every file in the tree is named
# by the manifest, so nothing can be ADDED after the scan. Read-only: it takes
# no lock, creates nothing and deletes nothing, which is what makes it safe to
# run from the uploader. Paths are printed repository-relative, so its own
# output can be shown in a published status file.
verify_publication_tree() {
  local rc=0
  python3 /dev/fd/3 "$ROOT" "${PUBLISH_DIR#"$ROOT"/}" 3<<'PYVERIFY' || rc=$?
import hashlib
import os
import stat
import sys

root, publish_rel = sys.argv[1], sys.argv[2]
publish = os.path.join(root, publish_rel)
problems = []
CONTROL = {"PUBLICATION-STATUS.txt", "MANIFEST.txt", "NOTICE.txt"}


def relative(path):
    return os.path.relpath(path, root)


def read_bytes(path):
    """The file's bytes, read no-follow through its own descriptor."""
    handle = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    try:
        if not stat.S_ISREG(os.fstat(handle).st_mode):
            raise OSError("not a regular file")
        with os.fdopen(handle, "rb") as reader:
            return reader.read()
    except BaseException:
        try:
            os.close(handle)
        except OSError:
            pass
        raise


if os.path.islink(publish) or not os.path.isdir(publish):
    print("VERIFY-PUBLICATION: REJECTED - %s is not a directory" % publish_rel)
    sys.exit(1)

status_path = os.path.join(publish, "PUBLICATION-STATUS.txt")
verdict = None
try:
    for line in read_bytes(status_path).decode("utf-8", "replace").splitlines():
        if line.startswith("PUBLICATION: "):
            verdict = line[len("PUBLICATION: "):]
            break
except OSError as error:
    problems.append("the status marker could not be read (%s)" % error)
if verdict is None:
    problems.append("the status marker states no verdict")
elif not verdict.startswith("APPROVED"):
    problems.append("the status marker says: %s" % verdict[:200])

# The manifest, last row per staged path: the report is staged again when the
# transaction needs a second pass, and the row that describes what is on disk
# is the last one written for it.
manifest_path = os.path.join(publish, "MANIFEST.txt")
expected = {}
withheld = 0
try:
    for line in read_bytes(manifest_path).decode("utf-8", "surrogateescape").splitlines():
        if not line or line.startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) < 5:
            problems.append("a manifest row has %d fields, not 5" % len(fields))
            continue
        staged, _, size, digest, _ = fields[0], fields[1], fields[2], fields[3], fields[4]
        if staged == "-":
            withheld += 1
            continue
        expected[staged] = (size, digest)
except OSError as error:
    problems.append("the manifest could not be read (%s)" % error)

found = set()
for directory, names, files_here in os.walk(publish, followlinks=False):
    for name in sorted(names):
        if os.path.islink(os.path.join(directory, name)):
            problems.append("%s is a directory symlink" % relative(os.path.join(directory, name)))
    for name in sorted(files_here):
        absolute = os.path.join(directory, name)
        relative_to_publish = os.path.relpath(absolute, publish)
        if os.path.islink(absolute):
            problems.append("%s is a symlink" % relative(absolute))
            continue
        if relative_to_publish in CONTROL or name.endswith(".WITHHELD.txt"):
            continue
        found.add(relative_to_publish)
        if relative_to_publish not in expected:
            problems.append("%s is in the tree but not in the manifest, so it was "
                            "added after the scan" % relative_to_publish)
            continue
        size, digest = expected[relative_to_publish]
        try:
            payload = read_bytes(absolute)
        except OSError as error:
            problems.append("%s could not be read (%s)" % (relative_to_publish, error))
            continue
        actual = hashlib.sha256(payload).hexdigest()
        if actual != digest:
            problems.append("%s does not match the digest that was approved"
                            % relative_to_publish)
        elif str(len(payload)) != size:
            problems.append("%s is %d bytes, not the %s that were approved"
                            % (relative_to_publish, len(payload), size))

for missing in sorted(set(expected) - found):
    problems.append("%s is named by the manifest but is not in the tree" % missing)

print("tree            : %s" % publish_rel)
print("verdict         : %s" % (verdict or "none"))
print("manifest rows   : %d published, %d withheld" % (len(expected), withheld))
print("files verified  : %d" % len(found))
print("problems        : %d" % len(problems))
for problem in problems:
    print("  PROBLEM %s" % problem)
if problems:
    print("VERIFY-PUBLICATION: REJECTED - the tree is not the one that was approved")
    sys.exit(1)
print("VERIFY-PUBLICATION: OK - every published copy is the one that was scanned")
PYVERIFY
  return "$rc"
}

# prune_inherited_evidence - examines what an earlier run left in this
# checkout's evidence trees, takes any credential-bearing artifact out of
# every publishable path, and then EMPTIES those trees and replaces the gate
# report an earlier run left behind with this run's in-progress stub. All four
# publication paths are therefore this run's from here on, not three of them.
#
# It is called from `init_output_tree`, before this run has written a single
# artifact of its own, and that ordering is the whole point. Deciding file by
# file whether something is recent enough to be this run's cannot be made
# sound - a modification time is metadata that any process with write access
# sets to any value it likes, so "newer than the run started" is a guess
# about provenance rather than a fact about it (CWE-345). Starting from an
# empty tree needs no guess: everything found in it afterwards was produced
# during this run. The staleness and future-date checks at staging time
# remain, as the second, independent statement of the same fact.
#
# What the sweep found, for `main` to act on: a non-zero
# status means an earlier run left an artifact carrying a credential
# signature, a link or an unreadable file in this checkout's evidence trees.
# How many passes the finalization transaction needed, whether anything was
# withheld or quarantined on the way, and whether the report reached the
# published tree. REPORT_WRITTEN says a report exists locally; this says one
# was published, which is a different fact and the terminal state of the
# transaction.
PUBLICATION_ATTEMPTS=0
PUBLICATION_INCIDENT="no"
REPORT_PUBLISHED="no"

PRE_RUN_SCAN_RC=0
PRE_RUN_QUARANTINED=0
PRE_RUN_SCAN_SUMMARY=""
PRE_RUN_SCAN_STATE="not run"

prune_inherited_evidence() {
  # A private holding directory OUTSIDE the three evidence roots, because the
  # roots are about to be emptied and the record of what was found in them has
  # to outlive that.
  local holding="$TARGET_DIR/.gate-inherited"
  local summary="$holding/publication-secret-scan-pre-run.txt"
  local guard

  ensure_output_dir "$holding" 0700 || return 1
  if ! guard="$(fs_guard prune "$ROOT" "$holding" 2>&1)"; then
    path_fatal "the holding directory for the inherited-evidence sweep could not be emptied (${guard:-no reason given})"
    return 1
  fi

  # The scan runs before the prune, so that a credential an earlier run left
  # behind is REPORTED and quarantined rather than quietly deleted. It needs
  # python3, which preflight has not yet verified; when python3 is absent the
  # scan is skipped and the prune below still removes everything, so nothing
  # inherited can reach the publication tree either way, and preflight stops
  # the run immediately afterwards with the reason.
  if command -v python3 >/dev/null 2>&1; then
    PRE_RUN_SCAN_STATE="ran"
    PRE_RUN_SCAN_RC=0
    scan_publication_artifacts pre-run allow-missing allow-empty \
      "$summary" "${PUBLICATION_PATHS[@]}" || PRE_RUN_SCAN_RC=$?
    if [[ "$PRE_RUN_SCAN_RC" -ne 0 ]]; then
      quarantine_scan_offenders "$summary" "the evidence an earlier run left behind"
      PRE_RUN_QUARANTINED="$QUARANTINED_COUNT"
    fi
  else
    PRE_RUN_SCAN_STATE="skipped: python3 is not on PATH"
  fi

  # The evidence roots are emptied, keeping the directories themselves so the
  # identities validated above stay valid. This is what makes provenance a
  # fact rather than an inference: every artifact found in these trees
  # afterwards was produced during this run, so publication does not have to
  # ask whether a timestamp looks recent enough to be trusted (CWE-345).
  if ! guard="$(fs_guard prune "$ROOT" "$PARITY_DIR" "$TEST_REPORT_DIR" "$AUDIT_DIR" 2>&1)"; then
    path_fatal "the evidence trees of earlier runs could not be cleared (${guard:-no reason given}), so this run cannot establish that the evidence it publishes is its own"
    return 1
  fi
  printf 'inherited evidence: cleared (%s); pre-run scan %s\n' \
    "$guard" "$PRE_RUN_SCAN_STATE"

  # The record of the sweep is this run's own evidence, so it moves into the
  # audit tree now that the tree is empty.
  if [[ -f "$summary" ]]; then
    if guard="$(fs_guard copy-tree "$ROOT" "$holding" "$AUDIT_DIR" 2>&1)"; then
      PRE_RUN_SCAN_SUMMARY="${AUDIT_DIR#"$ROOT"/}/publication-secret-scan-pre-run.txt"
    else
      printf 'WARNING: the record of the inherited-evidence sweep could not be kept (%s).\n' \
        "${guard:-no reason given}" >&2
      PRE_RUN_SCAN_SUMMARY=""
    fi
  fi
  if ! guard="$(fs_guard rmtree "$ROOT" "$holding" 2>&1)"; then
    printf 'WARNING: the holding directory %s could not be removed (%s).\n' \
      "${holding#"$ROOT"/}" "${guard:-no reason given}" >&2
  fi

  # The fourth publication path, and the last piece of inherited state: the
  # gate report itself. The three trees above are emptied; this one cannot be,
  # because the report is the deliverable and a run has to leave one behind -
  # so it is REPLACED, now that the scan above has read whatever an earlier
  # run left there, by a stub that belongs to this run.
  #
  # Two things were wrong without it, and both are about a figure or a
  # sentence describing a run other than the one being read. The boundary
  # row's enforced scan walks PUBLICATION_PATHS, of which this file is the
  # first: whether a previous run's report happened to be on disk moved that
  # row's artifact count by one and its "publication paths scanned" line
  # between "3 of 4" and "4 of 4", so two otherwise identical runs reported
  # different figures for the same tree. And until `write_report` renames the
  # finished report into place - minutes later, at the end of the run -
  # anyone reading target/gate-report.md saw the PREVIOUS run's complete
  # verdict, including its RESULT line, with nothing to say it was stale.
  # A stub fixes both at once: the count is this run's by construction, and a
  # mid-run reader is told the run is still going.
  if ! {
    printf '# Acceptance gate report\n\n'
    printf -- '- run id: %s, started (UTC) %s\n' "$RUN_ID" "$RUN_STARTED_UTC"
    printf -- '- script: %s\n\n' "$SCRIPT_NAME"
    printf 'RESULT: IN PROGRESS - run %s is measuring this checkout now, and this\n' "$RUN_ID"
    printf 'file will be replaced by its report when it finishes. The report of any\n'
    printf 'earlier run was cleared when this one started, together with the\n'
    printf 'evidence trees, so that nothing here can be read as a verdict on a run\n'
    printf 'other than the one named above. A run that is interrupted replaces this\n'
    printf 'stub with a partial report saying how far it got; a stub still here\n'
    printf 'afterwards means the run was killed before it could write either.\n'
  } | guarded_write "$REPORT_FILE"; then
    path_fatal "the gate report ${REPORT_FILE#"$ROOT"/} could not be replaced by this run's in-progress stub, so an earlier run's verdict would stay readable there and this run's artifact counts would depend on it"
    return 1
  fi

  # Recorded here rather than in `main`: this is where the finding was made,
  # and a row recorded now survives every later abort - including a preflight
  # failure, which would otherwise publish a report that never mentioned it.
  if [[ "$PRE_RUN_SCAN_RC" -ne 0 ]]; then
    record_blocking_row "Blocking - evidence left by the previous run" \
      "an earlier run's artifacts did not pass the secret scan; $PRE_RUN_QUARANTINED of them were quarantined out of every publishable path" \
      "${PRE_RUN_SCAN_SUMMARY:-none}" \
      blocking
  fi
  return 0
}


row_20_repository_boundary() {
  new_evidence repository-boundary.txt
  local failed=0
  local status
  local rc=0

  status="$(git status --porcelain -- "${PROTECTED_PATHS[@]}" 2>&1)" || rc=$?
  {
    printf '## Repository boundary - the Maven tree and governance files are untouched\n'
    command_line "git status --porcelain -- ${PROTECTED_PATHS[*]}"
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

  # -- the committed state, against the pinned baseline --------------------
  #
  # What HEAD contains, which the status above cannot see. Each protected
  # path's object id at HEAD must equal the id pinned in this script: a
  # committed change anywhere beneath it produces a different id, and a path
  # that has been deleted or replaced by a file of another kind produces none
  # at all, which fails here rather than passing as "nothing modified".
  local pinned path expected actual
  local mismatched=0
  printf '\n## The committed state of the protected paths, against the pinned baseline\n' >>"$EV"
  for pinned in "${PROTECTED_PATH_IDS[@]}"; do
    path="${pinned%%:*}"
    expected="${pinned#*:}"
    rc=0
    actual="$(git rev-parse --verify "HEAD:$path" 2>&1)" || rc=$?
    if [[ "$rc" -ne 0 ]]; then
      printf '# %s: NOT PRESENT at HEAD (git rev-parse exited %s: %s)\n' "$path" "$rc" "$actual" \
        >>"$EV"
      detail "the protected path $path is not present at HEAD"
      mismatched=$((mismatched + 1))
      continue
    fi
    if [[ "$actual" != "$expected" ]]; then
      printf '# %s: CHANGED -- pinned %s, at HEAD %s\n' "$path" "$expected" "$actual" >>"$EV"
      detail "the protected path $path has been changed in a commit (object $actual, pinned $expected)"
      mismatched=$((mismatched + 1))
      continue
    fi
    printf '# %s: unchanged (%s)\n' "$path" "$actual" >>"$EV"
  done
  if [[ "$mismatched" -ne 0 ]]; then
    failed=1
  fi

  # -- the committed state, against a baseline ref -------------------------
  #
  # The same property read a second way, from history rather than from a pin.
  # The BASELINE COMMIT ITSELF must be an ancestor of HEAD - not its merge
  # base with HEAD, which is an ancestor of HEAD by definition and would make
  # the test say nothing - because a baseline HEAD does not descend from
  # cannot bound what HEAD changed. The diff is then taken from that ancestor
  # commit to HEAD over the protected paths and must be empty.
  #
  # Every candidate is tried in order: the first that resolves AND is an
  # ancestor becomes the baseline. A candidate that resolves and is not an
  # ancestor is recorded, and if no candidate qualifies while at least one
  # resolved, the row FAILS - a diverged, rewritten or ahead-of-HEAD ref is a
  # baseline this history cannot be measured against, which is a result and
  # not a reason to pass. A checkout where no candidate resolves at all is
  # reported, and the pinned ids above decide the row.
  local baseline_ref="" baseline_commit="" candidate="" ref
  local rejected=""
  for ref in "${PROTECTED_BASELINE_REFS[@]}"; do
    rc=0
    candidate="$(git rev-parse --verify --quiet "$ref^{commit}" 2>/dev/null)" || rc=$?
    if [[ "$rc" -ne 0 || -z "$candidate" ]]; then
      continue
    fi
    if git merge-base --is-ancestor "$candidate" HEAD; then
      baseline_ref="$ref"
      baseline_commit="$candidate"
      break
    fi
    rejected="${rejected:+$rejected; }$ref ($candidate) is not an ancestor of HEAD"
  done

  printf '\n## The committed state of the protected paths, against a baseline ref\n' >>"$EV"
  if [[ -n "$rejected" ]]; then
    printf '# refs that resolved but do not bound this history: %s\n' "$rejected" >>"$EV"
  fi
  if [[ -z "$baseline_ref" && -n "$rejected" ]]; then
    {
      printf '# no candidate of %s both resolves and is an ancestor of HEAD, so no\n' \
        "${PROTECTED_BASELINE_REFS[*]}"
      printf '# ref-based bound on the committed state could be established\n'
    } >>"$EV"
    detail "no baseline ref is an ancestor of HEAD ($rejected), so the committed boundary cannot be bounded by history"
    failed=1
  elif [[ -z "$baseline_ref" ]]; then
    {
      printf '# none of %s resolves in this checkout, so the pinned object ids above\n' \
        "${PROTECTED_BASELINE_REFS[*]}"
      printf '# are the whole of the committed-state check\n'
    } >>"$EV"
  else
    local boundary_diff
    rc=0
    boundary_diff="$(git diff --name-status "$baseline_commit" HEAD -- "${PROTECTED_PATHS[@]}" 2>&1)" ||
      rc=$?
    {
      printf '# baseline ref: %s (%s), verified an ancestor of HEAD\n' \
        "$baseline_ref" "$baseline_commit"
      command_line "git diff --name-status $baseline_commit HEAD -- ${PROTECTED_PATHS[*]}"
      printf '# git exit status: %s\n' "$rc"
      printf '# output (empty is the pass):\n%s\n' "$boundary_diff"
    } >>"$EV"
    if [[ "$rc" -ne 0 ]]; then
      detail "the baseline-to-HEAD diff of the protected paths failed with status $rc"
      failed=1
    elif [[ -n "$boundary_diff" ]]; then
      detail "$(printf '%s\n' "$boundary_diff" | wc -l | tr -d ' ') protected path(s) changed between $baseline_ref and HEAD"
      failed=1
    fi
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
    # The same encoding the report itself uses, for the same reason and one
    # more: this file is tab-separated, and a value carrying a tab or a
    # newline would silently become two fields or two records - so what the
    # scan reads would not be what the report is composed from, which is the
    # only property this file exists to have. `markdown_cell` turns a tab into
    # a space and a newline into a visible marker, so every record here stays
    # one record.
    printf 'script\t%s\n' "$(markdown_cell "$SCRIPT_NAME")"
    printf 'root\t%s\n' "$(markdown_cell "$ROOT")"
    printf 'jdk\t%s\n' "$(markdown_cell "${JDK_VERSION:-unknown}")"
    printf 'sbt\t%s\n' "$(markdown_cell "${SBT_VERSION:-unknown}")"
    printf 'automated\t%s of %s expected\n' "$GATE_COUNT_AUTOMATED" "$GATE_EXPECTED_AUTOMATED"
    printf 'failed\t%s\n' "$GATE_COUNT_FAILED"
    printf 'reported\t%s\n' "$GATE_COUNT_REPORTED"
    printf 'run completed\t%s\n' "$RUN_COMPLETED"
    for index in "${!GATE_LABEL[@]}"; do
      printf '%s\t%s\t%s\t%s\t%s\n' \
        "$(markdown_cell "${GATE_LABEL[$index]}")" \
        "$(markdown_cell "${GATE_STATUS[$index]}")" \
        "$(markdown_cell "${GATE_DETAIL[$index]}")" \
        "$(markdown_cell "${GATE_COMMAND[$index]}")" \
        "$(markdown_cell "${GATE_EVIDENCE[$index]}")"
    done
  } | guarded_write "$material"

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
    detail "modules, examples, eclipse, pom.xml, src and .github are unchanged in the working tree and at HEAD (pinned object ids${baseline_ref:+, and no change since $baseline_ref}); $(awk -F': *' '/^  files /{ print $2 }' "$scan_summary") published artifact(s) carry no credential signature"
  fi
  return "$failed"
}

#-----------------------------------------------------------------------------
# The report.
#
# Written on every exit path through the EXIT trap, because it is the
# deliverable evidence and a run that ended early still has to say how far it
# got. Deterministic except for the one timestamp in its header and the run
# identity beside it, neither of which any gate compares.
#
# It is built in a temporary file beside the final one, verified, SCANNED and
# only then renamed - a rename within one directory is atomic, so a reader
# either sees the previous report or a complete new one, never half of either.
# `REPORT_WRITTEN` is set only after that rename succeeds: marking the report
# written before the write completes is what would let an interruption leave a
# partial file that the EXIT trap then declines to replace.
#
# THE SCAN COMES BEFORE THE RENAME, and that order is the point. The report
# quotes row details, tool output and file paths, so it can carry a credential
# signature that none of its sources carried on its own; it used to be renamed
# into place and marked written first, with the scan of its bytes happening
# afterwards - so a finding could only increment a counter while the published
# report still said every gate had passed (CWE-345). Now the assembly is
# scanned while it is still a temporary file: a finding is recorded as a
# blocking row, the report is reassembled WITHOUT the appendices - the part
# that quotes everything else, and therefore the part a signature almost
# certainly came from - and the reassembly is scanned in turn. Only an
# assembly that scans clean is renamed onto target/gate-report.md, and only a
# rename that succeeded marks the report written. A report that cannot be made
# clean is not published at all.
#
# Every count in it is tallied from the recorded verdicts by `gate_counts`,
# and none is derived by subtracting one running total from another. Counting
# the entries of the verdict arrays is what keeps the figures consistent with
# each other: a blocking row that is not one of the nineteen measured ones -
# a preflight failure - is recorded with its own kind and counted under that
# kind, rather than shifting a total it was never part of.
#-----------------------------------------------------------------------------

# assemble_report <temporary file> - writes the whole report into <file>.
#
# Separate from `write_report` because it is called TWICE on the path that
# matters: once to produce the report, and again after a credential signature
# in that first assembly has been recorded as a blocking row, so that the
# reassembly carries the row and drops the appendices. Every count it prints
# is re-tallied on each call, which is what makes the second assembly
# describe the run including its own publication failure.
assemble_report() {
  local temp_file="$1"

  gate_counts || true
  local index
  local rc=0

  if ! safe_truncate "$temp_file"; then
    printf 'WARNING: the gate report cannot be written to %s\n' \
      "${REPORT_FILE#"$ROOT"/}" >&2
    return 1
  fi

  {
    printf '# Acceptance gate report\n\n'
    # Every interpolated value goes through `markdown_cell`, and NONE of them
    # is wrapped in backticks. Both halves are necessary, and the second one
    # is the part that is easy to get wrong: `markdown_cell` escapes a
    # backtick as \` and a pipe as \|, which is correct in prose and in a
    # table cell - but CommonMark does not process backslash escapes INSIDE a
    # code span, so an escaped value written as `%s` between backticks can
    # still be ended by a backtick of its own and go on rendering as
    # structure. These values are the output of external tools, a path this
    # script did not choose and a git branch and subject that accept
    # backticks, so they are written as plain text and the inline-code
    # delimiters are kept for the fixed literals of this script alone.
    printf -- '- script: %s\n' "$(markdown_cell "$SCRIPT_NAME")"
    printf -- '- repository root: %s\n' "$(markdown_cell "$ROOT")"
    printf -- '- JDK: %s\n' "$(markdown_cell "${JDK_VERSION:-unknown}")"
    printf -- '- sbt: %s\n' "$(markdown_cell "${SBT_VERSION:-unknown}")"
    printf -- '- run finished (UTC): %s\n' \
      "$(markdown_cell "$(date -u '+%Y-%m-%dT%H:%M:%SZ')")"
    # The identity of what was measured. Without it this report is
    # indistinguishable from one written days and several commits ago - which
    # is exactly how a stale report comes to be read as current evidence.
    printf -- '- run id: %s, started (UTC) %s\n' \
      "$(markdown_cell "${RUN_ID:-unknown}")" \
      "$(markdown_cell "${RUN_STARTED_UTC:-unknown}")"
    printf -- '- commit: %s on %s - %s\n' \
      "$(markdown_cell "${HEAD_COMMIT:-unknown}")" \
      "$(markdown_cell "${HEAD_BRANCH:-unknown}")" \
      "$(markdown_cell "${HEAD_SUBJECT:-unknown}")"
    printf -- '- working tree at the time of the run: %s\n' \
      "$(markdown_cell "${HEAD_STATE:-unknown}")"
    printf -- '- provenance: every row below is a row of the validation table of the\n'
    printf '  technical specification (AAP section 0.10.1), executed in that order.\n\n'

    printf '## Gates\n\n'
    # The Command column states what each row RAN, beside its verdict: the
    # validation table defines every row as a measurement command, so a report
    # that gave the verdict and the detail alone left the reader to find the
    # command in this script - or in the row's evidence file, where five rows
    # did not write one at all. Each cell holds the same string the row's
    # evidence prints on its `# command:` line, because `command_line` writes
    # both from one argument.
    printf '| # | Gate / Rule | Verdict | Detail | Command | Evidence |\n'
    printf '|---|-------------|---------|--------|---------|----------|\n'
    if [[ "${#GATE_LABEL[@]}" -eq 0 ]]; then
      printf '| - | (no row completed) | - | the run ended before any row finished | none | `none` |\n'
    fi
    # Every cell is encoded by `markdown_cell`, which depends on no external
    # tool for the reason given at its definition - and on all four values,
    # not just the two that used to have their cell separator escaped: a
    # row's status is fixed text but its evidence path is not, and a newline
    # or a backtick in a detail forges a row just as effectively as a pipe
    # forges a column.
    for index in "${!GATE_LABEL[@]}"; do
      # The evidence path is a value a row supplied, so it is not wrapped in
      # backticks either, for the reason given at the header above - and the
      # command cell least of all: it quotes grep patterns, sbt commands and
      # javap invocations, which carry pipes, backticks and dollar signs of
      # their own and are exactly what `markdown_cell` is for.
      printf '| %s | %s | %s | %s | %s | %s |\n' \
        "$((index + 1))" \
        "$(markdown_cell "${GATE_LABEL[$index]}")" \
        "$(markdown_cell "${GATE_STATUS[$index]}")" \
        "$(markdown_cell "${GATE_DETAIL[$index]}")" \
        "$(markdown_cell "${GATE_COMMAND[$index]}")" \
        "$(markdown_cell "${GATE_EVIDENCE[$index]}")"
    done

    printf '\nGATES: %s total, %s passed, %s failed (automated rows; plus %s reported row)\n' \
      "$GATE_COUNT_AUTOMATED" "$GATE_COUNT_PASSED" "$GATE_COUNT_FAILED" "$GATE_COUNT_REPORTED"
    if [[ "$GATE_COUNT_PREFLIGHT_FAILED" -gt 0 ]]; then
      printf 'PREFLIGHT: FAILED - the required toolchain was incomplete, so none of the\n'
      printf '%s automated rows ran. The row above says which tools were missing.\n' \
        "$GATE_EXPECTED_AUTOMATED"
    fi
    if [[ "$GATE_COUNT_BLOCKING_FAILED" -gt 0 ]]; then
      printf 'BLOCKING: %s check(s) outside the validation table failed and stopped the\n' \
        "$GATE_COUNT_BLOCKING_FAILED"
      printf 'run. The toolchain is not what failed: the row(s) above name the check and\n'
      printf 'point at its evidence.\n'
    fi
    if [[ -n "$GATE_ARRAY_PROBLEM" ]]; then
      printf 'BOOKKEEPING: %s - the counts above cannot be trusted.\n' \
        "$(markdown_cell "$GATE_ARRAY_PROBLEM")"
    fi
    local unattributed=$(($(framework_error_count) - FRAMEWORK_ERRORS_ATTRIBUTED))
    if [[ "$unattributed" -gt 0 ]]; then
      printf 'FRAMEWORK: %s unchecked command failure(s) outside any row (see `%s`).\n' \
        "$unattributed" "$(markdown_cell "${FRAMEWORK_ERROR_FILE#"$ROOT"/}")"
    fi

    if [[ "$GATE_COUNT_PREFLIGHT_FAILED" -gt 0 ]]; then
      printf '\nRESULT: INCOMPLETE - preflight failed before any automated row could run,\n'
      printf 'so nothing was measured. This is not an acceptance result.\n'
    elif [[ "$GATE_COUNT_BLOCKING_FAILED" -gt 0 ]]; then
      printf '\nRESULT: INCOMPLETE - a blocking check outside the validation table failed,\n'
      printf 'so this is not an acceptance result. %s of the %s automated rows were\n' \
        "$GATE_COUNT_AUTOMATED" "$GATE_EXPECTED_AUTOMATED"
      printf 'measured, %s of them failed.\n' "$GATE_COUNT_FAILED"
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
    if [[ "$REPORT_SANITIZED" == "yes" ]]; then
      printf '\n(WITHHELD: the first assembly of this report carried a credential\n'
      printf 'signature, so it was reassembled without its appendices. The appendices\n'
      printf 'are the part that quotes row evidence, tool output and file paths, and\n'
      printf 'they are therefore where such a signature comes from. See the blocking\n'
      printf "row above and \`target/audit/publication-secret-scan-assembly.txt\`, which\n"
      printf 'names the rule and the line and withholds the value.)\n'
    elif [[ -s "$(guarded_fd_path "$APPENDIX_FD")" ]]; then
      # Read through the descriptor the appendices were written through, not
      # by name: the report is composed from these bytes, so a file
      # substituted at that name would put content this run never produced
      # into the published report (CWE-59 leading to CWE-345).
      cat "$(guarded_fd_path "$APPENDIX_FD")"
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
    printf -- "- \`target/publish/\` - the ONLY tree CI uploads: a sanitized, scanned\n"
    printf "  copy of each of the four above, with \`PUBLICATION-STATUS.txt\` stating\n"
    printf "  whether it was approved and \`MANIFEST.txt\` listing what was staged,\n"
    printf '  what was redacted from it and what was withheld\n'
    printf -- "- \`target/quarantine/\` - artifacts withheld from publication because\n"
    printf '  they matched a credential signature; never uploaded\n'
  } | guarded_write "$temp_file" || rc=$?

  if [[ "$rc" -ne 0 || ! -s "$temp_file" ]]; then
    printf 'WARNING: the gate report could not be assembled (status %s); %s is\n' \
      "$rc" "${REPORT_FILE#"$ROOT"/}" >&2
    printf 'left as it was, and this run will try again on its way out.\n' >&2
    # Best effort, and deliberately not a checked command: the failure that
    # matters has already been reported above.
    command rm -f -- "$temp_file" 2>/dev/null || true
    return 1
  fi
  return 0
}

# scan_report_assembly <file> <phase> - scans an assembled report before it is
# published, and distinguishes the two ways that can go wrong.
#
#   0  the assembly carries no credential signature
#   1  it does - or the scan could not complete over it (an unreadable file,
#      a refused symlink), which is the same verdict: not publishable
#   2  the scan itself could not RUN, so nothing is known about the bytes
#
# Status 2 exists because `python3` is one of the tools `preflight` checks
# for, and the report that says so must still be written: a run that cannot
# scan is one whose report is kept locally and NOT published, which is a
# different outcome from one whose report is known to carry a secret.
scan_report_assembly() {
  local file="$1"
  local phase="$2"
  local summary="$AUDIT_DIR/publication-secret-scan-$phase.txt"
  local rc=0

  scan_publication_artifacts "$phase" require-paths require-files "$summary" \
    "${file#"$ROOT"/}" >/dev/null 2>&1 || rc=$?
  if [[ ! -f "$summary" ]]; then
    REPORT_SCAN_DETAIL="the report assembly could not be scanned at all (the scanner wrote no summary)"
    return 2
  fi
  local findings
  findings="$(command awk -F': *' '/^findings /{ print $2; exit }' "$summary")"
  if [[ "$rc" -eq 0 ]]; then
    REPORT_SCAN_DETAIL=""
    return 0
  fi
  REPORT_SCAN_DETAIL="${findings:-an unreadable file or a refused symlink} credential signature(s) in the assembled report (rule and line in ${summary#"$ROOT"/}, value withheld)"
  return 1
}

# What the most recent report scan had to say; set by `scan_report_assembly`.
REPORT_SCAN_DETAIL=""

write_report() {
  # `write_report recompose` rebuilds a report that has already been written,
  # which is what the finalization transaction needs: a row recorded while
  # publishing has to appear in the report that gets published, and the first
  # composition happened before that row existed.
  if [[ "${1:-}" != "recompose" && "$REPORT_WRITTEN" == "yes" ]]; then
    return 0
  fi

  local temp_file="$REPORT_FILE.partial"
  assemble_report "$temp_file" || return 1

  # The scan, BEFORE the rename. See the section comment above for why this
  # order is the whole point of it.
  local scan_status=0
  scan_report_assembly "$temp_file" assembly || scan_status=$?
  case "$scan_status" in
    0) ;;
    1)
      record_blocking_row "Publication - the assembled gate report carries a credential signature" \
        "$REPORT_SCAN_DETAIL; the report was reassembled without its appendices" \
        "target/audit/publication-secret-scan-assembly.txt"
      REPORT_SANITIZED="yes"
      assemble_report "$temp_file" || return 1
      local recheck=0
      scan_report_assembly "$temp_file" assembly-recheck || recheck=$?
      if [[ "$recheck" -ne 0 ]]; then
        printf 'FATAL: the gate report cannot be made publishable: %s\n' \
          "${REPORT_SCAN_DETAIL:-the reassembly was not scanned}" >&2
        printf 'It is NOT written to %s, and the scan summaries under %s name the\n' \
          "${REPORT_FILE#"$ROOT"/}" "${AUDIT_DIR#"$ROOT"/}" >&2
        printf 'rule and the line without the value.\n' >&2
        # The assembly itself now carries the signature, so it is moved out of
        # target/ into the quarantine rather than left beside the deliverable
        # for something else to pick up. Detection acts on the artifact here
        # exactly as it does in the publication step.
        local quarantined
        if quarantined="$(fs_guard quarantine "$ROOT" \
          "$QUARANTINE_DIR/${RUN_ID:-unknown-run}" "${temp_file#"$ROOT"/}" 2>&1)"; then
          printf 'The assembly has been quarantined under %s (%s).\n' \
            "${QUARANTINE_DIR#"$ROOT"/}/${RUN_ID:-unknown-run}" "$quarantined" >&2
        else
          printf 'WARNING: the assembly at %s could NOT be quarantined (%s); remove it\n' \
            "${temp_file#"$ROOT"/}" "${quarantined:-no reason given}" >&2
          printf 'by hand.\n' >&2
        fi
        return 1
      fi
      ;;
    *)
      record_blocking_row "Publication - the gate report could not be scanned" \
        "${REPORT_SCAN_DETAIL:-the scanner did not run}; the report is written locally but will NOT be published" \
        "target/audit/preflight.txt"
      # Reassembled so that the report on disk states the row that has just
      # been recorded. It is deliberately not re-scanned: the scanner is what
      # could not run.
      assemble_report "$temp_file" || return 1
      ;;
  esac

  # The rename: atomic inside one directory, so no reader ever sees a
  # half-written report, and an interrupted attempt leaves only the temporary
  # file behind.
  # Not `mv`: `mv -f file directory` moves the file into the directory and
  # exits 0, so a directory at the report's name would leave this announcing
  # a report it had not published. The guarded rename refuses that, and it
  # publishes the inode that was just scanned rather than a re-read of it.
  if ! fs_guard rename "$ROOT" "$temp_file" "$REPORT_FILE" >/dev/null 2>&1; then
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
  # The report and the publication, from whatever exit path this is, as the
  # one transaction: a normal finish has already run it (and this is then a
  # no-op reading its verdict), while an aborted, failing or interrupted run
  # reaches it only here - and it has produced just as much for CI to upload.
  # Nothing leaves this machine that has not been sanitized and scanned,
  # whichever way the run ended, and nothing is marked published before the
  # scan that governs the uploaded bytes has passed.
  finalize_publication
  # A publication that was not approved is a failure of the run even when
  # every gate row passed: the artifacts are the deliverable and they could
  # not be handed over safely. Only a success status is overridden - a run
  # that was already failing keeps the status it had, and an interrupted one
  # keeps its 130/143. The override is APPLIED below, after the lock is
  # released, because it can only be applied by an explicit `exit`.
  local override="no"
  if [[ "$status" -eq 0 && "$PUBLICATION_APPROVED" != "yes" ]]; then
    printf 'FATAL: every gate passed but the evidence could not be published safely;\n' >&2
    printf 'exiting non-zero. See %s\n' "${PUBLICATION_STATUS_FILE#"$ROOT"/}" >&2
    override="yes"
  fi
  # Last, and only after the report exists: the lock. Releasing it before the
  # report was written would let a run waiting on it start writing into the
  # same evidence tree while this one was still publishing from it. This is a
  # no-op unless THIS run acquired the lock, so a run that stopped because
  # another holds it cannot remove that one's lock on its way out. Its status
  # is not propagated for the same reason `write_report`'s is not: `errexit`
  # is off here and it has already reported on stderr.
  release_output_lock
  # The override, last: `return` from an EXIT trap does NOT change the
  # process's exit status - the status the shell was already exiting with
  # wins - so an explicit `exit` is the only thing that can apply it, and it
  # must come after the lock has been released and the report and publication
  # are on disk. Bash does not re-enter the EXIT trap, so this cannot recurse.
  if [[ "$override" == "yes" ]]; then
    exit 1
  fi
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
  # The mode, decided by the one function that holds the argument contract -
  # so that an invocation this script does not understand is refused here just
  # as it is refused inside `init_run`, and a surplus word after a flag that
  # IS understood cannot be discarded on the way past. `classify_invocation`
  # returns rather than exits, precisely so that it can be read here through a
  # command substitution without the exit being swallowed by the subshell.
  local invocation
  invocation="$(classify_invocation "$@")" || exit 2

  # One mode runs no gate: the uploader's integrity check. It is intercepted
  # before `init_run` deliberately - it must take no lock, create nothing and
  # empty nothing, because it runs while and after another step owns the tree.
  if [[ "$invocation" == "verify-publication" ]]; then
    set -uo pipefail
    export LC_ALL=C
    resolve_locations
    require_repository_root || exit 2
    verify_publication_tree
    exit $?
  fi

  # Every side effect of a real run - the shell options, the exports, the
  # argument contract, the working directory, the output tree and the traps -
  # happens here and nowhere else.
  init_run "$@"

  printf '%s\n' '============================================================'
  printf 'acceptance gates for the Scala port of strata-collect/strata-basics\n'
  printf 'repository root: %s\n' "$ROOT"
  printf '%s\n' '============================================================'

  preflight

  # The controls over this framework's own behaviour, before the first row.
  # They need python3, which preflight has just confirmed, and they fail the
  # run closed: evidence produced by a framework that cannot be shown to
  # encode, scan, record and publish as specified is not evidence.
  if ! run_framework_self_checks; then
    # The status is not propagated: this path exits 2 for the self-check
    # failure whatever the publication does, and `finalize_publication`
    # records and prints its own verdict either way.
    if ! finalize_publication; then
      printf 'The evidence of this aborted run was not approved for publication.\n' >&2
    fi
    exit 2
  fi

  # Whatever a previous run left in the evidence trees was examined and then
  # cleared by `prune_inherited_evidence`, inside `init_output_tree` and
  # before this run wrote anything of its own into them. Two properties come
  # out of that, and both are load-bearing here:
  #
  #   * provenance is a fact rather than an inference - every artifact in
  #     those trees was produced during this run, so publication never has to
  #     ask whether a timestamp looks recent enough (CWE-345); and
  #   * a credential-bearing leftover was quarantined out of every publishable
  #     path at that point, and recorded as a blocking row THERE, so it
  #     survives even an abort that happens before this line.
  #
  # What is left for this branch is the decision: a finding in inherited
  # evidence stops the run, because the operator has to know that this
  # checkout was holding one.
  if [[ "$PRE_RUN_SCAN_RC" -ne 0 ]]; then
    printf 'FATAL: the evidence an earlier run left behind carried a credential\n' >&2
    printf 'signature, a symlink or an unreadable file. It has been taken out of\n' >&2
    printf 'every path this run or CI publishes (%s artifact(s) quarantined) and\n' \
      "$PRE_RUN_QUARANTINED" >&2
    printf 'this run stops rather than continuing in a checkout that was holding\n' >&2
    printf 'one. See %s\n' "${PRE_RUN_SCAN_SUMMARY:-the pre-run scan record}" >&2
    # The report and the publication of whatever is safe to publish, as one
    # transaction - the EXIT trap would reach it in any case, and doing it
    # here keeps this path's ordering explicit. Exit 2 stands for the
    # inherited-evidence finding regardless of the publication's verdict.
    if ! finalize_publication; then
      printf 'The evidence of this aborted run was not approved for publication.\n' >&2
    fi
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
  run_gate "Gate 5 / Rule 4 - JVM construction and serialization closure" row_11a_jvm_closure
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
  # The prescribed WORDING is fixed. The specification states what this row
  # says - "automated checks passed; manual approval: see PR review" - and
  # `gate7_manual_detail` prints that sentence verbatim on every run, so a
  # reader or a tool looking for it always finds it. What that function adds,
  # and only when the automated half did not pass, is a qualifier naming that
  # row's actual verdict: the prescribed sentence opens by asserting that the
  # automated checks passed, and a table in which one row asserts that while
  # the row above it says FAIL tells a reader something untrue about this run.
  # Nothing is concealed either way - the automated half is its own PASS/FAIL
  # row immediately above, counted in the summary line and in the exit status,
  # and the evidence file this row points at is that row's evidence.
  #
  # The verdict and the evidence path are both read positionally - the row
  # that has just run is the last element of each array - so that inserting a
  # row above cannot silently make this one describe a different row.
  local last=$((${#GATE_STATUS[@]} - 1))
  record_reported_row "Gate 7 - migration note (manual approval)" \
    "$(gate7_manual_detail "${GATE_STATUS[$last]:-absent}")" \
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

  # Composing the report, staging it, and verifying everything that would be
  # uploaded are ONE transaction (`finalize_publication`). Its failures are
  # recorded as blocking rows BEFORE the report it publishes is composed, so
  # the published report states them and the tally below counts them - which
  # is what the older arrangement could not do: it discovered a publication
  # failure after the report had already said everything passed, and then
  # incremented a counter the report knew nothing about.
  # Its failure is already a blocking row of the table and a line on stderr,
  # and the tally below counts it, so the status is not re-tested here.
  if ! finalize_publication; then
    printf 'The evidence was not approved for publication; the report says so.\n' >&2
  fi

  # The tally, taken again now that the transaction has recorded whatever it
  # had to record. Every count printed below and every count in the report
  # comes from the same five arrays, so the two cannot disagree.
  if ! gate_counts; then
    printf 'FATAL: %s\n' "$GATE_ARRAY_PROBLEM" >&2
    GATE_FAILED=$((GATE_FAILED + 1))
  fi
  recorded_errors="$(framework_error_count)"
  unattributed_errors=$((recorded_errors - FRAMEWORK_ERRORS_ATTRIBUTED))
  if [[ "$unattributed_errors" -gt 0 && "$RUN_COMPLETED" == "yes" ]]; then
    printf 'FATAL: %s unchecked command failure(s) recorded while publishing;\n' \
      "$unattributed_errors" >&2
    printf 'see %s\n' "${FRAMEWORK_ERROR_FILE#"$ROOT"/}" >&2
    GATE_FAILED=$((GATE_FAILED + 1))
    RUN_COMPLETED="no"
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
