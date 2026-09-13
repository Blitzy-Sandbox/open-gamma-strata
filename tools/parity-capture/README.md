Parity Capture
--------------
This directory holds the capture procedure for the **Java-generated parity baselines** of the
Scala port of `strata-collect` and `strata-basics`.

It contains exactly two files:

* `capture-baseline.jsh` — a JShell script that runs against the Maven-built *Java* Strata jars
  and emits six numerical parity fixtures and one reference-data manifest.
* `README.md` — this document: the procedure, the fixture schemas and the manifest schema.

The **seven JSON documents** the script emits are the deliverable, and they belong in the two
Scala modules' test resources. **All seven are committed**: the five `strata-basics` parity
fixtures, the `strata-collect` one and `reference-data-manifest.json` — listed with their paths in
section 4 and their schemas in sections 6 and 7. The script is retained so that the baselines can
be **regenerated and audited** by a third party, and this README is what that third party follows.

**`java-test-mapping.csv` is not one of them.** The method-level traceability document that sits
beside the manifest in `strata-basics/src/test/resources/manifest/` is neither written nor read by
this capture. It records mapping decisions no scanner can derive — which Scala spec absorbed a
consolidated Java test, why a test was dropped — so it is authored and owned alongside the Scala
test suite and verified by the test-scope row of `scripts/verify-gates.sh`, which consumes it.
Section 7 describes its format for the reader who needs it; nothing in this procedure produces it,
so a capture neither depends on a hand-authored committed document nor republishes one.

No user-specified rules govern this file, so it is held to enterprise-standard best practice
instead: every number is traceable to a file, nothing is claimed that was not observed, and each
command is recorded with whether it was run here. The *Verification record* in section 3 lists the
ones that were — with tool versions, exit statuses, document sizes and hashes, and the outcome per
document — and says plainly which one was not; the limitations subsection next to it draws the
boundary of what the script and the committed set actually guarantee, and where a property holds by
construction rather than by measurement, the text says so instead of citing a run.

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
`strata-basics`. `tools/` is on neither: it is on no sbt source root, and it is compiled by
nothing. No CI job runs it. `.circleci/config.yml` carries the Maven jobs and the Scala job
`scala_build21` — the job key under the file's `jobs:` map, wired in under `workflows:` → `build:` →
`jobs:`, both of which `grep -n scala_build21 .circleci/config.yml` prints in that order (the
definition first, the wiring second). Those two anchors are named rather than numbered on purpose:
the job body is edited far more often than this document is, so a line range here goes stale without
anything a reader can see changing — which is how an earlier revision came to point at a comment
inside the job. The job installs sbt 1.13.0 on the JDK 21 executor and runs
`scripts/verify-gates.sh`; that script drives `sbt` and mentions
neither Maven nor JShell nor this script anywhere —
`grep -cE '\bmvn\b|jshell|capture-baseline' scripts/verify-gates.sh` answers `0`. So CI asserts the
Scala port against the **committed** documents and never regenerates them: a capture is always a
deliberate local act, run by the procedure in section 3. The Java
implementation being measured depends on Guava and Joda, so removing them here does not improve
purity; it only makes the capture impossible.

**The procedure writes nowhere near the Java tree, and the script enforces that rather than
trusting you — what no check can cover is which directory *outside* the checkout you point it
at.** The acceptance gate requires
`git status --porcelain -- modules examples eclipse pom.xml src .github` to stay empty. The Maven
build writes its jars into `target/`, which is git-ignored (`.gitignore:13`); the canonical `install`
form of step 1 *additionally* writes the shared local Maven repository, which is outside the
checkout and outside that gate but not without consequence — step 1 and the safety note at the end
of section 3 state what it changes and when to prefer `package`. The script writes exactly the
seven **relative** paths in section 4, resolved
against the output root: `guardedOutputTarget` checks each one at run time, rejecting an absolute
path, any path containing `..` or a backslash, any path whose first segment is `modules`,
`examples`, `eclipse`, `src`, `.github` or `project`, and any path that is not one of the seven
declared destinations. The root those paths are resolved against is vetted too:
the root comes from the `parity.out.dir` system property (default `.`), and `outputRoot` **refuses
a root that sits inside a checkout without being that checkout's root before it creates
anything** — so a root such as `modules/probe` is refused *and leaves no directory behind* rather
than quietly writing a `strata-basics/…` tree where the boundary gate forbids one — then rejects a
symbolic link at **any** component of it, creates the missing components one at a time, requires
the result to equal its own canonical form, and re-asserts the placement on the canonical path,
because canonicalisation can move a path into a checkout. The one other thing a run writes — an
empty lock file under the JVM temporary directory — is held to the same boundary: that directory
must be absolute, must already exist and must lie outside every checkout, or the run refuses
(section 3, *Output root, temporary directory and locking*). Run from the repository root with the
default, or point the output root at a scratch directory you own (section 3), and confirm with
`git status` afterwards. The Java
sources, tests and resources under `modules/**` are read-only reference material: they are the
source every later migration slice ports from, and the only reproducible source of these
baselines. Never edit them to make a fixture match.

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
3. commit the documents it emits — all seven are in the tree today (section 4);
4. let the Scala specs assert against them — every consumer exists today: six parity specs, the
   harness they share, the manifest spec and the gate runner that collects their reports
   (section 4).

Two consequences follow, and both matter more than they look.

* **A failing parity spec is never fixed by editing a fixture.** The fixture is the Java answer.
  Either the Scala code is wrong (fix the code), or the fixture is stale because the Java tree
  changed (regenerate it with this procedure and review the diff). Hand-editing a value destroys
  the only independent evidence the port has.
* **The capture cross-checks itself.** Captured values are compared against the hard-coded Java
  test constants and the structural invariants that section 6 itemises fixture by fixture, and
  every reference-data row count is compared against an independently verified expected count. The
  run reports how many checks each fixture contributed and counts separately the entries that have
  no Java constant to compare against. Every check runs *before* any file is written, so a mismatch
  aborts the run with a non-zero status having written nothing at all, and a document on disk is
  one that agreed with the checks the script ran when it was produced. That check coverage is not
  uniform across fixtures — section 6 states it per fixture, and the summary table in section 3
  gives the measured counts.

### 2. Prerequisites

* **JDK 21.** `jshell` is part of the JDK, so nothing else needs installing. The repository's CI
  already provides a JDK 21 executor (`.circleci/config.yml:36-38`, image `cimg/openjdk:21.0`).
* **Apache Maven**, to build the two Java modules.
* **A POSIX-compatible shell on a Unix-like platform** — bash, zsh, dash or equivalent. Every
  command below is POSIX shell — `$(…)` substitution, `trap … EXIT`, quoted variable expansion,
  `for`/`if` blocks — and every classpath is **colon**-separated, which is the JVM's separator on
  Unix only. On Windows, run them under WSL or another POSIX shell; a native `cmd`/PowerShell
  session needs both the quoting and the classpath separator (`;`) translated, and this document
  does not give that translation.
* **A clean checkout**, with the shell's working directory at the **repository root** — the paths
  below, including the script's default output root and the script's own path, are
  repository-root relative. The capture reads the Java implementation and its reference data from
  the **classpath**, never from the checkout, so the checkout supplies only those paths: capturing
  into a scratch root works from the repository root with `-R-Dparity.out.dir` (section 3).
* **A usable temporary directory.** The run keeps one empty lock file directly under
  `java.io.tmpdir`, which must be absolute, must already exist and must lie outside every checkout;
  the default `/tmp` on a Unix-like platform satisfies all three. The capture creates no directory
  there — only that one file, and only relative to a handle on a directory that already exists. A platform whose default is reached through a symbolic
  link (macOS, where `/var` links to `/private/var`) is fine — the script resolves it — but a
  `java.io.tmpdir` you override yourself is held to those three rules and refused otherwise
  (section 3, *Output root, temporary directory and locking*).
* Roughly 1 GB of free heap for the capture JVM (`-R-Xmx900m` is what the script is run with), and
  disk for the documents: the seven committed documents occupy **24,302,650 bytes** at this
  revision, and a capture writes exactly those bytes again — they *are* what a capture writes.
  Capturing into a scratch root beside them — which is how section 3 verifies a run — therefore
  needs both, so allow **at least 60 MB** free. Re-measure that total whenever a document's
  coverage changes; `wc -c` over the seven paths section 4 lists is the whole measurement, and the
  per-document sizes are in the *Verification record* of section 3.

No network access is needed once Maven's dependencies are cached, and no database, service or
container is involved.

### 3. Procedure

#### Step 1 — build the two Java modules

The **canonical prerequisite**, and the one the project plan specifies — **for a machine whose local
Maven repository is yours alone.** It publishes into that repository, so on a shared or CI host use
alternative 1 instead, or redirect the publication as alternative 1 shows; the difference is spelled
out there rather than left to be discovered:

```
mvn -B -q -pl modules/collect,modules/basics -am -DskipTests -Dcheckstyle.skip=true \
    -Dmaven.javadoc.skip=true install
```

Either form of step 1 produces

* `modules/collect/target/strata-collect-2.12.74-SNAPSHOT.jar`
* `modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar`

The coordinates are the Maven build's own: version `2.12.74-SNAPSHOT` (`pom.xml:13`), artifactIds
`strata-collect` (`modules/collect/pom.xml:11`) and `strata-basics`
(`modules/basics/pom.xml:11`). Both outputs land under git-ignored `target/`.

**Alternative 1 — `package` instead of `install`.** Replace the last word with `package` (and drop
`-q` if you want the reactor summary):

```
mvn -B -pl modules/collect,modules/basics -am -DskipTests -Dcheckstyle.skip=true \
    -Dmaven.javadoc.skip=true package
```

`package` leaves the jars in git-ignored `target/` and writes nothing else. The canonical `install`
goes further: it copies both jars into the **shared local Maven repository**
(`~/.m2/repository/com/opengamma/strata/…`) under the *mutable* `2.12.74-SNAPSHOT` coordinates that
every other build on the machine resolves, overwriting whatever generation was there. That is
harmless on a dedicated machine and is what makes route (a) and route (c) below work at all, but on
a shared or CI cache it changes an input of builds that are not yours. Prefer `package` (route (b))
unless you want that publication.

If you want the canonical `install` spelling itself on a shared machine — to rehearse it, or because
a reviewer asks for the documented command rather than its alternative — give it a repository of its
own and the publication lands there:

```
mvn -B -q -pl modules/collect,modules/basics -am -DskipTests -Dcheckstyle.skip=true \
    -Dmaven.javadoc.skip=true -Dmaven.repo.local="$PARITY_SCRATCH/m2" install
```

`-Dmaven.repo.local` moves *the whole* local repository for that invocation, so the copy has to be
able to resolve the build's dependencies: seed it with `cp -a ~/.m2/repository "$PARITY_SCRATCH/m2"`
(a plain copy — never `cp -al`, whose hard links would put the shared originals back in the
publication's path), or leave it empty and let Maven re-resolve from the network. That is exactly
how the *Verification record* below exercised this line without touching the shared repository.

**Alternative 2 — no build at all.** If both jars are already in the local repository, route (c) of
step 2 skips step 1 entirely — with the provenance caveat that route states.

#### Step 2 — assemble the capture classpath

The script needs six jars: the two module jars **plus** the Java implementation's third-party
dependencies (Guava, `failureaccess`, Joda-Beans, Joda-Convert). Three routes supply them, and they
differ in where each one comes from. The two Maven-resolved routes hand the capture more than those
six — `dependency:build-classpath` emits everything it resolves for the module, so at this revision
route (b)'s classpath is 18 entries: the six required plus twelve test-scope JUnit/AssertJ/Mockito
jars. The extra entries are harmless, because the script loads only the classes it imports; route
(c) names the six directly and carries nothing else.

**Route (b) is the one to use for an audit**, and the reason is narrow: its two *module* jars are
built by step 1 from the checkout in front of you, so the Java behaviour being measured is that
revision's. The third-party entries come from the local Maven repository on every route — they are
version-pinned by `modules/pom.xml` and not built here. Route (a) is the shortest to type and
publishes the module jars on the way; route (c) is the fastest and takes its module jars on trust
unless you check them (that route says how). The *Verification record* below states exactly which
routes were run here and what came out.

Two of the routes write Maven's resolved classpath to a file first. Create that file in a **private
scratch directory you own**, never at a fixed path under `/tmp`: a predictable shared name can
collide with another run on the same machine, can be pre-created or symlinked by another user, and
what lands in it is fed straight to `--class-path`. `mktemp -d` gives a private directory. Use a
dedicated variable, **fail closed** if `mktemp` fails — an empty variable would turn `"$VAR/out"`
into the root-level path `/out` — and install the cleanup `trap` only once the directory exists:

```
PARITY_SCRATCH="$(mktemp -d)" || exit 1
trap 'rm -rf -- "$PARITY_SCRATCH"' EXIT
```

The variable is deliberately not one a caller is likely to have already set, and it is assigned
unconditionally rather than defaulted, so the `trap` can never be pointed at a directory the shell
inherited. Quote every path derived from it, as the routes below do.

Route (a) — `install`, then let Maven resolve the whole basics classpath:

```
mvn -B -q -pl modules/collect,modules/basics -am -DskipTests -Dcheckstyle.skip=true \
    -Dmaven.javadoc.skip=true install
mvn -q -pl modules/basics dependency:build-classpath \
    -Dmdep.outputFile="$PARITY_SCRATCH/basics-cp.txt"
CP="$(cat "$PARITY_SCRATCH/basics-cp.txt"):modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar"
```

This is the shortest form, and it works because `modules/basics/pom.xml` already depends on
`strata-collect` at compile scope (`:19-22`) and on its test-jar (`:50-55`). Note the reason
`install` is required here: `dependency:build-classpath` resolves `strata-collect` from the local
repository even when `-am` is passed, so after `package` alone a machine that has never installed
`strata-collect` cannot resolve it. The price is the one step 1 names — this route republishes both
mutable SNAPSHOT artefacts into the shared local repository — so on a machine whose `~/.m2` other
builds read, take route (b).

Route (b) — `package` only, resolving just the third-party jars and naming the module jars
explicitly:

```
mvn -q -pl modules/collect dependency:build-classpath \
    -Dmdep.outputFile="$PARITY_SCRATCH/collect-cp.txt"
CP="modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar"
CP="$CP:modules/collect/target/strata-collect-2.12.74-SNAPSHOT.jar"
CP="$CP:$(cat "$PARITY_SCRATCH/collect-cp.txt")"
```

`modules/collect` has no OpenGamma dependencies, so its resolved classpath is purely third-party —
the four jars the capture needs plus the module's test-scope jars. The two entries named explicitly
above are the ones that matter: they are step 1's output from this checkout, which is why this is
the route to audit with.

Route (c) — zero rebuild, no temporary file at all. If both module jars are already in the local
Maven repository, the classpath is exactly six entries, named directly:

```
M2="${HOME}/.m2/repository"
CP="$M2/com/opengamma/strata/strata-basics/2.12.74-SNAPSHOT/strata-basics-2.12.74-SNAPSHOT.jar"
CP="$CP:$M2/com/opengamma/strata/strata-collect/2.12.74-SNAPSHOT/strata-collect-2.12.74-SNAPSHOT.jar"
CP="$CP:$M2/com/google/guava/guava/33.4.0-jre/guava-33.4.0-jre.jar"
CP="$CP:$M2/com/google/guava/failureaccess/1.0.2/failureaccess-1.0.2.jar"
CP="$CP:$M2/org/joda/joda-beans/2.11.1/joda-beans-2.11.1.jar"
CP="$CP:$M2/org/joda/joda-convert/2.2.3/joda-convert-2.2.3.jar"
```

The versions are the Java build's own — Guava `33.4.0-jre` and Joda-Convert `2.2.3` / Joda-Beans
`2.11.1` (`modules/pom.xml:768,771-772`), with `failureaccess` arriving as Guava's own dependency —
and step 1 can be skipped entirely on this route, which is its whole point: it touches neither the
shared local repository nor a temporary file.

**What route (c) does not establish.** `2.12.74-SNAPSHOT` is a mutable coordinate, so those two
jars are whatever the last `install` on this machine put there — a different revision, a dirty tree,
or another checkout altogether. The script's classpath preflight (step 3) proves only that the
classes it needs can be **loaded**; it reads no version, revision or hash, so it cannot tell you
which sources they were compiled from. Measuring the wrong revision this way produces a perfectly
clean run and a baseline that pins the wrong numbers, which is why route (b) is the audit route.
Use route (c) for a convenience re-run, or after establishing provenance — both jars record the
commit they were built from, so this is two commands and a comparison:

```
jar xf "$M2/com/opengamma/strata/strata-basics/2.12.74-SNAPSHOT/strata-basics-2.12.74-SNAPSHOT.jar" \
    META-INF/MANIFEST.MF
grep -E 'Implementation-(Build|Date)' META-INF/MANIFEST.MF
```

(run it inside a scratch directory: `jar xf` extracts into the working directory. `unzip -p` does
the same where it is installed.) `Implementation-Build` is a git commit hash. The jars are usable
for a capture of this checkout when `git diff --stat <that commit>..HEAD -- modules` is empty —
`modules/**` being the only source they are compiled from — and are not otherwise. Repeat the check
for the `strata-collect` jar. Measured here: both jars reported
`Implementation-Build: 39c46e342a4a95ac083d66287f038f6ae276692a`, the diff of `modules` against the
measured `HEAD` (`65dd1e28ddb9604323b7f94e7530e2ff4bf54a2a`) was empty, and route (c) then produced
documents byte-identical to route (b)'s — evidence for this revision, and an illustration of the
check rather than a substitute for it.

#### Step 3 — run the capture

```
jshell --class-path "$CP" -R-Xmx900m tools/parity-capture/capture-baseline.jsh
```

Before anything else the script runs a **classpath preflight** over the types it needs. An
incomplete classpath is the most common mistake, and the preflight puts an actionable message
**first** — naming the missing classes and the build command that fixes them — instead of leaving
JShell's "package does not exist" errors as the only diagnostic. It does not suppress them, and it
cannot end the run where it stands: a JShell command cannot be made conditional, so there is no
`/exit 1` to put inside an `if`, and `System.exit` inside a snippet does not propagate — it kills
the execution engine, whereupon JShell falls through to its REPL and terminates with status
**zero**. So the preflight prints and returns, JShell carries on loading the snippets that follow,
and the Strata imports a few lines later fail exactly as they would have. Read the *first* block of
output rather than the last.

The run fails closed regardless, and how it does is worth knowing, because the obvious construction
does not work. The status is **not** computed in the exit expression. The capture publishes its
verdict into the `capture.exit.status` property in one statement it has to reach and earn — every
document built, every check passed — and the script's last statement is
`/exit (Integer.getInteger("capture.exit.status", 1))`, an expression that names nothing but
`java.lang.Integer` and defaults to **failure**. The driver re-checks the preflight before building
anything, so nothing is written either. An incomplete classpath is precisely the case in which the
script's own declarations do not survive the load, so a verdict carried in a snippet-declared
symbol would be unreadable exactly when it carries the most information; a property is not. A value
you pass in yourself is reported and discarded rather than believed, so
`-R-Dcapture.exit.status=0` cannot buy a success.

One classpath shape needs a further layer, and the script carries it. A classpath holding the
Strata jars **without Guava** lets javac resolve the Strata signatures but not the Guava types
inside them, and on JDK 21.0.12.1 that combination makes **javac itself** fail with an internal
`NullPointerException` while generating code for one of the script's methods ("An exception has
occurred in the compiler … `Type.getTag()` because `type` is null"). From there every later snippet
is rejected with a spurious `cannot find symbol / symbol: class` pair — the exit expression
included, and a bare `/exit 1` with it, both measured — so no exit expression of any shape can be
evaluated and the run would fall through to the REPL and terminate zero. Two further JShell
behaviours recover it: a failed `/exit` does not end the file, JShell reads the next line; and
`/reset` is a command, needs no compilation, and discards the corrupted compilation context along
with every snippet. So the file's final two lines are `/reset` and the literal `/exit 1`, reached
only when the expression above could not be evaluated at all — which is only ever a run whose own
declarations did not survive, so nothing was built and nothing was written. Every classpath shape
in the *Verification record*'s table exits non-zero, including that one.

**Output root, temporary directory and locking.** The script writes the seven documents
**relative** to the value of the `parity.out.dir` system property, whose default is `.` — the
current directory, expected to be the repository root. It is the only property the capture itself
defines. Pass it through JShell's `-R` prefix to write elsewhere. Capturing into a
directory you own and then comparing is the safe way to inspect a run before it touches the
working tree, and it is how the *Verification record* below was produced:

```
OUT="$PARITY_SCRATCH/out"
jshell --class-path "$CP" -R-Xmx900m -R-Dparity.out.dir="$OUT" \
       tools/parity-capture/capture-baseline.jsh
```

`$PARITY_SCRATCH` is the private directory step 2 created. On route (c), which skips step 2, create
it here with the same two fail-closed lines before the block above:

```
PARITY_SCRATCH="$(mktemp -d)" || exit 1
trap 'rm -rf -- "$PARITY_SCRATCH"' EXIT
```

Both properties of those two lines matter: `|| exit 1` stops the shell if `mktemp` fails, rather
than letting an empty variable turn `"$PARITY_SCRATCH/out"` into `/out`, and the unconditional
assignment means the recursive `rm` in the `trap` can only ever delete a directory this shell
created.

**The lock file, and `java.io.tmpdir`.** A run takes one exclusive lock, keyed on the canonical
output root, and keeps it in an empty file named `parity-capture-baseline-<sha256 of the canonical
output root>.lock` directly under `java.io.tmpdir` — not in the output root, which is normally the
checkout and where a lock file would be an untracked artefact. The lock serialises two captures over
one output root: the second is refused rather than queued, naming the lock file and the root. The
run prints the file it took, so `output lock     = /tmp/parity-capture-baseline-<sha>.lock` in the
header is the path to look at if a capture reports contention.

That path is a write, so it is validated the way the output root is, and the standard JVM default
(`/tmp` on a Unix-like platform) already satisfies every rule. A `java.io.tmpdir` you set yourself
must be:

* **absolute** — a relative value would resolve against the working directory, which is normally
  the checkout, so it is refused rather than interpreted;
* **already present** — the script does not create a directory the caller named;
* **outside every checkout** — neither the value nor the path it resolves to may be a checkout root
  or sit inside one, so `-R-Djava.io.tmpdir=$PWD/modules` exits 1 naming the checkout it sits inside
  and creates nothing there.

A non-canonical value is resolved rather than refused — which is what makes the macOS default
`/var/folders/...`, reached through the `/var` link, work — and the resolved path is the one the run
uses. Every component of it is then opened relative to its parent with `NOFOLLOW`, so a symbolic
link anywhere in the path refuses the run instead of redirecting the write, and nothing on that
path is created: an absent component, or one that cannot be opened as a directory without following
a link, stops the run.

**Nothing in the lock path is created by pathname.** The lock file is created *through* the handle
on that directory, with owner-only permissions, which is why no private lock directory is made
first: Java has no handle-relative `mkdir` — a `SecureDirectoryStream` can open, move and delete,
but cannot create a directory — so a directory could only be made by naming its path, and a parent
renamed between the validation and that call would leave a directory created somewhere else. A file
needs no such call, so none is made. Whatever is at the lock name must then be **this run's own
regular file, writable by nobody else**: a symbolic link there (refused by the no-follow open
itself), a directory, a file owned by another account and a group- or other-writable file each stop
the run rather than being locked, because a file another account controls can be replaced under the
run. Ownership is established from the OS and not from the `user.name` property — a probe file
created through the same handle supplies this process's effective identity, and is removed again
whichever way the check goes. Every one of those refusals, and the parent-replacement race the
handle defeats, is measured in the *Verification record* below.

**Comparing a capture with what is committed.** Compare the exact documents, one at a time, in
both modules — not directory trees. `$OUT/strata-basics` holds only the generated
`src/test/resources/…` subtree, so `diff -r "$OUT/strata-basics" strata-basics` reports every
source and test file the capture never produces, and a `strata-basics`-only comparison misses the
`strata-collect` fixture altogether:

```
for f in strata-basics/src/test/resources/parity/daycount-baseline.json \
         strata-basics/src/test/resources/parity/schedule-baseline.json \
         strata-basics/src/test/resources/parity/fx-baseline.json \
         strata-basics/src/test/resources/parity/currency-math-baseline.json \
         strata-basics/src/test/resources/parity/holiday-baseline.json \
         strata-collect/src/test/resources/parity/double-array-baseline.json \
         strata-basics/src/test/resources/manifest/reference-data-manifest.json; do
  if [ ! -e "$f" ]; then
    echo "not committed: $f"
  elif cmp -s "$OUT/$f" "$f"; then
    echo "identical:     $f"
  else
    echo "DIFFERS:       $f"
  fi
done
```

`cmp -s` exits 0 when two files match, so the loop's own lines are the report; drop `-s` to have
`cmp` name the first differing byte. At this revision the loop prints `identical` for **all
seven** documents — exactly what the *Verification record* below measured. Anything else is a
finding: a `DIFFERS` line, or a `not committed` line for a document section 4 lists as committed,
is something to investigate before you replace a committed file.

**Exit status.** Zero only when every document was built *and* every self-check passed; non-zero
on any check failure, count mismatch, preflight failure or exception in the driver — and non-zero
as well when the load is broken enough that the script's own declarations never compile, which is
the case step 3 above describes the two layers for. Zero is stated positively, once, by a statement
the run must reach and earn; every other path, including every way of not reaching that statement,
leaves the default in place. The documents
are accumulated in memory and flushed only once every check has passed, so any failure that
happens **before the flush begins** — a failed check, a count mismatch, the preflight, or an
exception raised while the documents are being built — writes **nothing at all**: no document is
touched and none can be left half-valid.

A failed **check** prints one line per failure, always beginning `<fixture> | <row or counted
thing> | <detail>`, and the detail is as numeric as the check is. The double comparisons
(`checkClose`, and `checkExact` through it) print `expected`, `actual`, the absolute and relative
deltas, the tolerance and the name of the tolerance the Java test sets; `checkInt` prints expected,
actual and their difference; `checkCount` prints the expected and actual count; `checkEquals`
prints both values. The 29 `checkTrue` call sites assert a structural invariant rather than a number
— a unique `id`, an ascending date list, a row-shape rule — and print that invariant's own
description, which is what identifies the violation; there is no delta to print for them. The other
two failure paths have their own shapes and are not check lines at all: the classpath preflight
prints its own block (step 3) and an exception in the driver prints `ABORTED: the capture driver
threw …` with the message.

Once the flush begins the guarantee changes shape, and the distinction is worth reading carefully.
The flush is itself a transaction: it validates all seven destinations, writes each document to
`<target>.capture-tmp-<pid>.new` and verifies it by reading the bytes back, publishes by moving any
existing target aside to `.old` and renaming the verified temporary into place — both renames being
atomic within one directory — deletes the backups only once every target is published, and rolls
back to the previous generation on any failure before rethrowing, so the run still exits non-zero.
What it does not claim is a single atomic swap of all seven documents: a process **killed** between
two publish moves leaves a mixed generation on disk, which is the one case where a non-zero status
does not mean nothing was written. That state is never silent — the `.old` and `.new` files stay
where they are, the next run refuses to publish over them and names them, and `git status` shows
the same thing because the documents are tracked. Recovery is the same as for any failed run —
compare against what is committed with the loop above, then re-run. Limitation 2 below draws the
same boundary from the script's side.

**Progress output.** The run prints the JDK version, the resolved output root, the random seed and
the lock file it holds,
one line per document, then a summary table of `rows`, `checks`, `errRows` (rows whose expectation
is a Java failure) and `capOnly` (rows — or, in `currency-math`, expectation entries — deliberately
without a Java constant to check against; limitation 5 below records the one place that tally and
the fixture's own marker disagree), and finally the path and size of each document written.

**Safety.** Run every step from the repository root. The capture writes only the seven relative
paths listed in section 4, resolved against the output root, plus the one lock file described
above, and refuses any other relative path — including anything whose first segment is `modules`,
`examples`, `eclipse`, `src`, `.github` or `project`, as section 0 describes.

The root those paths resolve against admits exactly two shapes, and the script decides which it is
looking at rather than trusting the caller. **Inside a checkout, only the checkout root itself is
permitted**: a root with a checkout root among its ancestors is refused before a single directory
is created, so `parity.out.dir=modules/probe` and `parity.out.dir=$PWD/probe/nested` both exit 1
with `parity.out.dir is inside the checkout rooted at …` and leave nothing behind — measured, both
of them. An in-checkout root is therefore never a way to write into the Java tree; with the
checkout root itself, the seven destinations are exactly the committed fixture paths under
`strata-basics/` and `strata-collect/` — the files a capture exists to replace, and the only tracked
files it ever touches. What is left to your judgement is the other shape: an
**absolute root outside every checkout** is created and used exactly as given, because that is how
a dry run into a scratch directory works, so keep it inside a directory you own.

Maven's side effects are step 1's: `package` writes only git-ignored `target/`, while the canonical
`install` — and route (a), which needs it — also republishes both mutable SNAPSHOT jars into the
shared local Maven repository. Neither touches the Java sources.

Run `git status --porcelain -- modules examples eclipse pom.xml src .github` after any non-default
run anyway: empty is the acceptance gate, it is a one-second check, and it is cheaper than
reasoning about whether a particular root was one of the refused shapes.

#### Verification record

Everything in this subsection was measured, and it is pinned to one revision — the only way a
statement about a script's output stays checkable:

| Pinned to | Value |
|---|---|
| Measured against | the revision of the script whose hash is the next row |
| `capture-baseline.jsh` | sha256 `1fc8dc5f2df21a491066d222bad851f63f9da87884a3362bd83d9ea6fcb38ba6` |
| Java sources measured | `modules/**` at commit `86d8c8a32d73f6621b5b7ac901ad58a80d2257bd`, unchanged since `39c46e342a4a95ac083d66287f038f6ae276692a` (item 4 below) |
| Toolchain | OpenJDK 21.0.12.1 (Temurin 21.0.12.1+1 LTS), Apache Maven 3.9.16 |

The script hash is the anchor: check it with
`sha256sum tools/parity-capture/capture-baseline.jsh`, and if it no longer matches, re-run the
commands below rather than trusting the numbers that follow them.

Commands run, in this order:

1. Step 1 in its **`package`** form — `BUILD SUCCESS`, exit 0, 12.2 s against warm Maven caches,
   producing `strata-collect-2.12.74-SNAPSHOT.jar` and `strata-basics-2.12.74-SNAPSHOT.jar` under
   git-ignored `target/`. Their byte sizes are deliberately not quoted here: a jar's size moves with
   its manifest, so it is not a property worth pinning — what matters is that both files exist at
   the paths step 1 names and that the capture's preflight then finds every class it needs.
2. `dependency:build-classpath` for `modules/collect`, into a private scratch file — exit 0. The
   resulting route (b) classpath is **18 entries**: the six the capture needs (the two module jars
   named explicitly, plus Guava, `failureaccess`, Joda-Beans and Joda-Convert) and twelve
   test-scope JUnit/AssertJ/Mockito/byte-buddy jars the script never loads.
3. The capture **three times**, each into its own scratch output root: twice with the route-(c)
   classpath (the six jars named directly out of `~/.m2`) and once with the route-(b) classpath
   built in items 1-2 from this checkout. All three printed `all checks passed` and `capture
   complete.`, wrote seven documents and exited 0, at `-R-Xmx900m`. All three were timed:
   **54-57 s wall** on a busy machine — read that as the order of magnitude, not as a benchmark. The
   route-(b) run additionally carried `-R-Dcapture.exit.status=5`, which is the probe recorded
   below the failure table: it exited 0, so the preset was discarded rather than returned.
4. The provenance check route (c) asks for: `Implementation-Build` read from both local-repository
   jars' manifests — `39c46e342a4a95ac083d66287f038f6ae276692a` on each — and
   `git diff --stat 39c46e342a4a95ac083d66287f038f6ae276692a..HEAD -- modules`, which printed
   nothing. That is why item 3's route-(c) runs were measuring this checkout's Java sources and not
   another revision's.
5. `git status --porcelain -- modules examples eclipse pom.xml src .github` — no output, before and
   after every run, including the Maven build of item 1 and every refusal probe below.

Step 1's canonical `install` line **was** run, in the redirected form step 1 documents —
`-Dmaven.repo.local` pointed at a plain `cp -a` copy of the local repository — because this
environment's repository is shared with other builds and republishing the mutable
`2.12.74-SNAPSHOT` coordinates would change an input of theirs. It exited **0** and published
`strata-basics` and `strata-collect` (jars and test-jars) under
`com/opengamma/strata/…/2.12.74-SNAPSHOT/` inside that copy, while the shared repository's own two
jars stayed byte-identical — `sha256sum` and `stat` before and after, compared and equal. What that
does not measure is the publication landing in the *default* repository, which is the one thing the
redirect exists to avoid; route (a)'s classpath step then resolves against whichever repository the
install populated. Routes (b) and (c) were measured end to end.

**The three captures produced byte-identical documents** — `cmp` over the three output roots, all
seven documents, no differences, and each of the seven equal to the committed file as well. Two of
those runs differ only in being separate runs, which is the determinism contract of section 5
demonstrated rather than asserted; the third differs in where its jars came from, which is what
makes route (b)'s agreement with route (c) a measurement rather than an assumption. All three ran on
one JDK, and the limitations below say what that does not extend to.

The script's own summary, as printed by each run. `rows` and `checks` are what the script counted,
`errRows` the rows whose expectation is a Java failure, and `capOnly` the entries with no Java
constant to check against:

| Fixture | rows | checks | errRows | capOnly |
|---|---|---|---|---|
| `currency-math` | 6 | 230 | 29 | 228 |
| `daycount` | 18,356 | 147,239 | 1,072 | 17,497 |
| `double-array` | 43 | 382 | 0 | 24 |
| `fx` | 14 | 100 | 14 | 4 |
| `holiday` | 3,846 | 50,561 | 2 | 0 |
| `manifest` | 1 | 52 | 0 | 0 |
| `schedule` | 879 | 320 | 573 | 769 |
| **TOTAL** | **23,145** | **198,884** | | |

All seven committed documents are reproduced byte for byte; the comparison follows. The totals move
whenever a document is extended, so read them as this revision's reading rather than a fixed
figure, and note how
uneven the `checks` column is — `schedule` states 320 checks against 879 rows, because most of a
schedule row's expectations have no Java constant to check against and are counted under
`capOnly`.

What the run produced, against what is committed:

| Document | Captured bytes | Committed bytes | `cmp` | sha256 |
|---|---|---|---|---|
| `daycount-baseline.json` | 13,302,984 | 13,302,984 | identical | `52ec04ee…` |
| `schedule-baseline.json` | 1,179,362 | 1,179,362 | identical | `a313f9c2…` |
| `fx-baseline.json` | 63,920 | 63,920 | identical | `9c3f9d00…` |
| `currency-math-baseline.json` | 123,495 | 123,495 | identical | `350404ed…` |
| `holiday-baseline.json` | 9,287,428 | 9,287,428 | identical | `e71adc59…` |
| `double-array-baseline.json` | 90,821 | 90,821 | identical | `a9e5ff95…` |
| `reference-data-manifest.json` | 254,640 | 254,640 | identical | `a6d285dd…` |

The seven together are the **24,302,650 bytes** section 2 asks you to have free twice over, and the
capture reproduces all seven hash for hash. Every figure in these three tables moves when a
document's coverage changes, so refresh them together rather than one at a time.

The failure path was measured across **every** incomplete-classpath shape that can be assembled
from the six jars, not one of them. Measuring one is what let an earlier revision of this document
claim a guarantee the script did not yet have: the `strata-collect`-jar-alone shape it measured was
one of the shapes that did behave, while a classpath missing only Guava exited **0** having written
nothing — the failure mode step 3 now describes, and the reason the script ends in `/reset` and a
literal. In every row the preflight's twenty-two lines named the missing classes **first**, the run
exited **1**, the output root was **never created** and **zero** files were written:

| Classpath | Missing classes named | Exit | Files | How it exited |
|---|---|---|---|---|
| both Strata jars + Joda, **no Guava** | 7 | 1 | 0 | `/reset` + literal: javac crashed and the exit expression was rejected |
| the same shape, run again | 7 | 1 | 0 | identical, so the recovery is not incidental |
| the same shape + `-R-Dcapture.exit.status=0` | 7 | 1 | 0 | the preset was reported and discarded, then as above |
| both Strata jars + Guava, **no Joda** | 8 | 1 | 0 | exit expression; the property was never set |
| `strata-collect` jar alone | 12 | 1 | 0 | exit expression |
| `strata-basics` jar alone | 12 | 1 | 0 | exit expression (javac crashed; the context survived it) |
| everything but the `strata-basics` jar | 8 | 1 | 0 | exit expression |
| everything but the `strata-collect` jar | 10 | 1 | 0 | exit expression (javac crashed; the context survived it) |
| empty classpath | 13 | 1 | 0 | exit expression |

Three of those nine shapes crash javac; only the two Guava-less ones lose the exit expression with
it, which is why a shape-by-shape reading is the only safe one — "an incomplete classpath" is not
one behaviour. Output length ranged from 162 to 4,394 lines and is incidental: it moves with
JShell's diagnostics. The load-bearing properties are the four the table asserts — preflight
first, non-zero exit, output root never created, nothing written.

The converse was measured on the same script: the full classpath plus a hostile
`-R-Dcapture.exit.status=5` exited **0** with seven byte-identical documents, so clearing the
property discards a caller's value without costing a legitimate run its zero (had the preset been
believed, that run would have exited 5).

The write-boundary refusals were measured in the same session, each exiting 1 and leaving nothing
behind:

| Probe | Outcome |
|---|---|
| `-R-Dparity.out.dir=modules/probe` | refused, naming the checkout it sits inside; no `modules/probe` created |
| `-R-Dparity.out.dir=$PWD/probe/nested` | refused the same way; no `probe/` created |
| `-R-Djava.io.tmpdir=$PWD/modules` | refused, naming the checkout the lock would have landed in; nothing created there |
| `-R-Djava.io.tmpdir=modules` (relative) | refused as relative, before resolving it |
| `java.io.tmpdir` symlinked into a checkout | refused on the **resolved** path, which is the check the declared path cannot make |
| `java.io.tmpdir` that does not exist | refused rather than created |
| symbolic link planted at the lock **file** name | refused by the no-follow open (`Too many levels of symbolic links (NOFOLLOW_LINKS specified)`); the link target was never created |
| directory planted at the lock **file** name | refused (`Is a directory`); nothing was locked |
| lock file owned by another account (`chown nobody`) | refused, naming both owners |
| lock file made group- and other-writable (`chmod 666`) | refused, printing the permissions it found |
| `-R-Duser.name=<another name>` on an otherwise normal run | exit 0 and seven identical documents — the ownership check reads the OS, so the property neither defeats nor breaks it |
| the validated temporary directory **renamed** and its pathname replaced by a symbolic link into a checkout-shaped `modules/`, while the run held the handle | the lock file was created in the directory the handle held, **nothing** was created at the replacement target, and the run completed normally |

The last row is the one that needs a method note, because it is a race rather than a configuration.
It was run against a scratch copy of the script with the window between the validated handle and the
handle-relative create widened to 45 s, so the swap could be performed by hand inside it; the
committed script's window is the microseconds between two adjacent statements. With the swap in
place the run still created its lock in the renamed directory — the object the handle holds, which
no longer carries that name — and the checkout-shaped target it was redirected at stayed empty.
That is the property to re-check if the lock code changes: not that the run survives a swap, but
that a swap cannot make it write somewhere else.

#### Known limitations at this revision

Each item below is a property of the script and of the committed set at the revision pinned above,
not of the procedure in general, and each says how to check whether it still holds.

1. **Every committed document is regenerated byte for byte, so a `DIFFERS` line is a finding.** All
   seven — `daycount-baseline.json` included — come back identical from a run of the checked-in
   script, so a re-capture is a valid way to refresh any of them, and a document that comes back
   `DIFFERS` is something to investigate rather than the expected state. The day-count section
   emits the committed 18,356-row document with the uniform twelve-key row shape, a unique `id` on
   every row and `scheduleInfo` always an object. Check: capture into a scratch root and run the
   comparison loop above; all seven printing `identical` is the expected state.

2. **The flush is a transaction, but seven files in three directories cannot be replaced in one
   filesystem operation.** Documents are staged in memory and every check runs first, so a failure
   before the flush writes nothing. The flush itself then validates all seven destinations, takes a
   no-follow handle on each directory and refuses to proceed if an earlier run left artefacts
   there; writes each document to `<target>.capture-tmp-<pid>.new` with `CREATE_NEW` and verifies
   it by reading the bytes back; publishes by moving any existing target aside to `.old` and
   renaming the verified temporary into place, both renames being atomic within one directory; and
   rolls back to the previous generation on any failure. What it does not claim is a single atomic
   swap of the whole generation: a process **killed** between two publish moves leaves a mixed set
   on disk. That state is never silent — the `.old` and `.new` files stay where they are, the next
   run's phase 1 refuses to publish over them and names them, and `git status` shows the same thing
   because the fixtures are tracked. Check: read `flushDocuments` in the script, then compare and
   re-run as step 3 describes.

3. **The output root is validated, and an absolute root still writes wherever it points.**
   `guardedOutputTarget` vets the seven relative suffixes, and `outputRoot` vets the root they
   resolve against, in this order: a root inside a checkout that is not that checkout's root is
   refused **before any directory is created**, every component is then rejected if it is a
   symbolic link, the missing components are created one at a time rather than through
   `createDirectories`, the result must equal its own canonical form, and the placement is
   re-asserted on the canonical path, because canonicalisation can move a path into a checkout.
   The order matters and was measured: `-R-Dparity.out.dir=modules/probe` exits 1 with the
   "inside the checkout rooted at …" refusal, writes no file **and leaves no `modules/probe`
   directory behind**, and the same holds for a nested root such as `$PWD/probe/nested`; a nested
   root outside any checkout is still created and used, which is how the verification above
   captured into a scratch directory. What remains yours is the choice itself: an absolute root
   outside the repository writes there. Run the `git status --porcelain` check after any
   non-default run anyway — it is a one-second check and the boundary gate is what it protects.

4. **Byte-identity is demonstrated on one toolchain, not proved for every one — but no output
   order is left to the JVM.** Every order in the seven documents is declared or total. The
   day-count list is the literal `STANDARD_DAY_COUNT_NAMES` array transcribed from `DayCounts.java`
   in file order, and `DayCounts.class.getDeclaredFields()` is used only as a **completeness
   check**, with the declared and reflected name lists sorted before they are compared, so the
   unspecified order `getDeclaredFields()` returns never reaches a row. Every manifest constant
   array is likewise read reflectively for completeness and then **sorted by (canonical name, field
   name)** — a total order, the field name breaking the tie of two constants naming one value, such
   as `OvernightIndices.EUR_ESTER` and `EUR_ESTR`.
   So the residual limitation is evidential rather than structural: three runs on OpenJDK 21.0.12.1
   were byte-identical, and that is the evidence there is. Treat **any** ordering difference in a
   re-capture as a finding — including one confined to a constants array, which under this design
   means the source table or the sort changed, not that the JVM answered differently. Check: read
   `STANDARD_DAY_COUNT_NAMES` and `namedConstants` in the script, then re-capture and compare.

5. **The `capOnly` summary column counts more entries than carry the marker.** For
   `currency-math` the summary reports 228 while the fixture carries `"captureOnly": true` on 173
   of its 355 expectation entries: the 55 per-currency seeded-random amounts are counted without
   the field being emitted. The marker in the fixture is the contract a consumer reads; the column
   is a run-time counter. Do not derive one from the other.

6. **Nothing re-captures automatically, so a stale fixture is found by a person, not by CI.** Every
   consumer of these documents exists and runs (section 4), and `scripts/verify-gates.sh` fails
   when a parity report is missing or carries a failed row — but all of that asserts the port
   against the **committed** documents. CI does rebuild and test the Java modules: `maven_install`
   (`mvn install -T 4 -DskipTests -Dstrict`) and `maven_test` (`mvn test`) are defined at
   `.circleci/config.yml:66-80` and run by the `build`, `build11`, `build17` and `build21` jobs
   (`:245-281`). What no job does is couple that rebuild to a capture: none runs this script or
   compares a regenerated document with a committed one —
   `grep -cE '\bmvn\b|jshell|capture-baseline' scripts/verify-gates.sh` is 0, and `scala_build21`
   runs only that script. So a change under `modules/**` that should move a baseline produces green
   gates against the old numbers until someone re-captures. The countermeasure is procedural and
   cheap: when `modules/**` changes, re-run section 3 and review the diff. Check: `git log --oneline
   -1 -- modules` against the date of the last change to the seven documents.

### 4. Outputs

Seven documents, all UTF-8, LF-terminated, with exactly one trailing newline. The `Committed`
column is the state at this revision, not a plan:

| Path | Committed | Contents |
|---|---|---|
| `strata-basics/src/test/resources/parity/daycount-baseline.json` | yes | Day-count year fractions, relative year fractions and day counts, with and without schedule information |
| `strata-basics/src/test/resources/parity/schedule-baseline.json` | yes | `PeriodicSchedule` resolutions: unadjusted and adjusted dates, periods, stubs, resolved conventions |
| `strata-basics/src/test/resources/parity/fx-baseline.json` | yes | `FxMatrix` / `FxRate` rates, conversions, cross rates and merges |
| `strata-basics/src/test/resources/parity/currency-math-baseline.json` | yes | `CurrencyAmount`, `Money`, `BigMoney`, `MultiCurrencyAmount` and the two amount-array types |
| `strata-basics/src/test/resources/parity/holiday-baseline.json` | yes | Per-calendar, per-year holiday sets and date-arithmetic samples |
| `strata-collect/src/test/resources/parity/double-array-baseline.json` | yes | `DoubleArray` and `DoubleMatrix` operation results |
| `strata-basics/src/test/resources/manifest/reference-data-manifest.json` | yes | The reference-data manifest: every ported data table, enumerated from Java, with asserted counts |

One further document lives beside the manifest and is **not** produced here:
`strata-basics/src/test/resources/manifest/java-test-mapping.csv`, the method-level test
traceability record. It is authored with the Scala test suite and verified by the test-scope row of
`scripts/verify-gates.sh`; section 7 documents its format because this is where a reader looks for
it, not because a capture writes it.

Consumers — a schema change breaks these, so change both sides together. Every one of them exists
and runs at this revision; there is no planned-but-absent half of this list:

* One parity spec per fixture, each decoding its document and comparing Java expectations with
  Scala results: `parity/DayCountParitySpec.scala` (`daycount`), `ScheduleParitySpec.scala`
  (`schedule`), `FxParitySpec.scala` (`fx`), `CurrencyMathParitySpec.scala` (`currency-math`) and
  `HolidayCalendarParitySpec.scala` (`holiday`) under
  `strata-basics/src/test/scala/com/opengamma/strata/basics/`, plus
  `strata-collect/src/test/scala/com/opengamma/strata/collect/parity/DoubleArrayParitySpec.scala`
  (`double-array`), which lives in `strata-collect` because its subject does.
* `strata-basics/src/test/scala/com/opengamma/strata/basics/parity/ParityHarness.scala` — loads a
  fixture, applies the tolerance rule of section 6 and writes
  `<parity.report.dir>/<fixture>.json`; the five basics specs run through it, and the collect spec
  carries the same logic locally, since `strata-collect` cannot see `strata-basics` test classes.
  The harness has no spec of its own: AAP section 0.3.1 enumerates the harness and exactly five
  basics `*ParitySpec` files, so its contract is asserted from inside one of the specs that consume
  it. `DayCountParitySpec.scala` carries that section — it drives every harness decision through
  `ParityHarness.load` and `decodeRows` over a probe row model and over two committed baselines, so
  the schemas below are exercised independently of the comparisons that consume them.
* `strata-basics/src/test/scala/com/opengamma/strata/basics/ReferenceDataManifestSpec.scala` — the
  consumer of `reference-data-manifest.json`: it asserts that the ported Scala data objects equal
  the manifest, key by key and row by row (section 7).
* `scripts/verify-gates.sh` — the gate runner. Its Gate 3 row requires all six parity reports to be
  present with zero failed rows and copies their per-fixture counts into `target/gate-report.md`.
  Its test-scope row reads `java-test-mapping.csv`, the document this capture neither writes nor
  reads (section 7).

Every Scala consumer reads its resource through `collect.io.Resources.readClasspathText` and decodes
it with circe; the gate runner is a shell script and reads the parity reports and the mapping CSV
with shell tools instead. The circe decoders are derived at **compile time**, so each one fixes a
field-name and nesting expectation in the compiled test code — but that expectation meets these
documents only when the specs **run** and decode them: a renamed key or a flattened object compiles
perfectly and fails at decode. That is the sense in which sections 6 and 7 are a contract — a
statically derived decoder, enforced by the test run — and why a change to either side has to be
made on both.

All seven documents are the deliverable and are committed alongside the Scala code, and this script
is what regenerates every one of them, byte for byte.

### 5. Encoding conventions

These conventions are the contract across the Java→Scala boundary for the **seven JSON documents**
this capture writes. The capture writes them and the circe decoders read them, so neither side may
change one alone.

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
* **Determinism.** Every pseudo-random value comes from a `java.util.Random` seeded with the single
  literal `20240117` (the JDK specifies the algorithm, so it reproduces exactly). There are two such
  generators, and the split is deliberate: one shared generator supplies every fixture and is
  consumed in fixture order, so adding a draw to an earlier fixture shifts the values of every later
  one — which is why `currency-math` uses literal sweep amounts rather than draws (section 6) —
  while `double-array-baseline.json` has a second generator of its own, seeded from the same
  literal, so its 18 random rows are insulated from that coupling and stay put when another
  fixture's draws change. Beyond the seed: there is no wall-clock input; no `HashMap`/`HashSet`
  appears on any output path, only insertion-ordered or sorted maps; every serialized order is
  declared in the script or totally sorted, never taken from reflection (limitation 4 in section 3);
  object keys are written in a fixed declared order; strings are escaped to pure ASCII, so the bytes
  do not depend on the platform charset; indentation is two spaces, line endings are `\n`, and
  scalar arrays are chunked at a fixed ten items per line, so a long array is neither one unreadable
  line nor one line per element. Same inputs, same bytes: three runs produced byte-identical
  documents, as the *Verification record* shows.
* **`holiday-baseline.json` alone is written one row object per line**, each row compact on its own
  line — the array's own two-space indent still prefixes every one of those lines, so the document
  is `[`, then 3,846 lines of the form `  {"id":"gblo-1950",…}`, then `]`. What is dropped is the
  indentation and the newlines *inside* a row, not the line prefix. It is done because those rows
  carry about 445,000 date strings: pretty-printing them costs roughly 2 MB of pure indentation and
  buys nothing, since the unit anyone reads or diffs there is the row. It is the same deterministic
  writer (`JArray.writeRowsPerLine`, rendering each row through `Jn.writeCompact`) — key order,
  escaping and number rendering are unchanged, and the document still ends with exactly one
  newline. Every other JSON document is pretty-printed as described above.

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

Each fixture is a **JSON array of row objects**. In the five fixtures that carry one, a `source`
field names the Java test method or the generated population the row came from, which is what
makes a failure attributable — `daycount-baseline.json` (47 distinct values),
`schedule-baseline.json` (16), `fx-baseline.json` (14), `currency-math-baseline.json` (6) and
`holiday-baseline.json` (6). `double-array-baseline.json` carries no `source` key at all; its
provenance is encoded in its `id` prefixes instead (`javatest-*`, `random-*`, `ieee-*`).

**Row identity differs by fixture, and a decoder has to know which case it is in.** Five fixtures
carry an explicit, unique `id`, and that is the name a parity report prints:
`daycount-baseline.json` (on all 18,356 rows), `holiday-baseline.json` (all 3,846),
`fx-baseline.json` (all 14), `currency-math-baseline.json` (all 6) and
`double-array-baseline.json` (all 43). One does not:

* `schedule-baseline.json` has neither an `id` nor a distinct `source`: 879 rows share 16 `source`
  values, and `grid.combinations` alone accounts for 768 of them. A consumer therefore derives a
  row's identity from `source` **plus the eleven input fields *and* `replacedStartDate` where the
  row carries one**. That last part is not optional: over the committed fixture, `source` plus the
  eleven inputs yields **878** distinct identities for 879 rows, because two `data_replace` rows
  share one base definition and differ only in the replacement date (`2014-07-04`, which resolves,
  and `2014-09-04`, which is rejected). With `replacedStartDate` included, all 879 are distinct —
  measured, not assumed. A report that names only the `source` cannot say which of the 768 grid
  rows failed either, so put the frequency, stub convention, roll convention and calendar in the
  message. Adding an `id` to the fixture would be the better fix, and would mean regenerating it
  and updating this section together.

**An `error` is an expectation in its own right, not an omission** — but exclusivity is per
*operation*, not per row. A row that evaluates one operation carries either its value or its
`error`. A row that evaluates several carries one outcome per operation and may legitimately mix
them: in `daycount-baseline.json`, `yearFraction` and `error` are never both present, `days` and
`daysError` are never both present, yet **1,048 rows carry an `error` for one operation alongside a
successful value for another** — 1,028 where the year fraction fails and `days` succeeds, and the
20 out-of-order rows where `relativeYearFraction` succeeds because it has no order check while
`yearFraction` and `days` both reject the pair. Per-fixture sections below state which fields pair
with which error.

**Harness contract.** `ParityHarness.assertParity(label, actual, expected)` returns the
discrepancy, or nothing when the two values are at parity: at parity means `|a − e| ≤ 1e-9`
**and** `|a − e| ≤ 1e-9 · max(|a|, |e|, 1e-300)` — both bounds, so neither a large magnitude nor a
near-zero value can hide a discrepancy. The `label` is what makes a report readable, so it names
the field being compared. `assertParityOpt` compares values either side may be without (two
absences are at parity, one absence differs) and `assertParitySeq` compares lists element by
element. Bit-identical values, signed zero included, are at parity without arithmetic, and a
non-finite value is at parity only with an identical one. Dates, lists and strings compare
exactly. An `error` row must produce a `Left` from an `Either`-returning API, or the documented
`ArgCheck` exception from a precondition API. Each parity spec writes
`<parity.report.dir>/<fixture>.json` with its pass/fail counts *before* asserting that nothing
failed, so the counts survive a failing run, and `scripts/verify-gates.sh` collects all six reports
into `target/gate-report.md`, failing its Gate 3 row if one is missing or carries a failed row. The
reports come from the **ScalaTest run** — this script never writes them.

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
  otherwise (Java throws there; the port returns `None`). The Java value on such a row was computed
  with that `Schedule` itself as the `ScheduleInfo`, and the list is the boundary set that schedule
  holds, so the rule above is the port's reconstruction of `Schedule.getPeriodEndDate` rather than
  something the capture re-derives: the capture does not evaluate the row a second time through a
  list-only implementation, and it is `DayCountParitySpec` — which decodes `periodEnds` and answers
  `periodEndDate` from it — that establishes the reconstruction agrees.

`null` inside `scheduleInfo` has exactly one meaning: **absent**, i.e. `None` on the Scala side and
the Java `ScheduleInfo` default, so a day count that reads it raises. `eom` is the one exception a
reader needs to know: `null` there means the interface default, which is `true`. Since the
`DayCountTest` stub always supplies an explicit flag, `eom: null` is precisely the set of rows
evaluated through the Java **two-argument** overload against `DayCounts.SIMPLE_SCHEDULE_INFO` —
true by construction rather than by an assertion, because the capture selects that overload exactly
when it has no stub to pass — so the default-versus-stub distinction needs no discriminator field.

One consequence of that single `null` meaning is worth stating, because it is the only place the
fixture's input departs from the Java test's: `30E/360 ISDA` reads the schedule end date only in
`!secondDate.equals(scheduleInfo.getEndDate())`, and the Java stub passes `null` there — a null
that means "not the maturity date", not "absent". Four of the nineteen `test_…_notMaturity` rows
depend on it. Those rows therefore carry a **materialised** schedule end of `end + 1 day`, a date
that is provably not the maturity, so Java takes the same branch as it does for the stub's `null`,
and the captured value is asserted against the Java table's not-maturity column — the value the
Java test asserts for its own stub evaluation. That is the whole of the substitution: no other row
is re-evaluated under a second `ScheduleInfo` implementation, and nothing here claims that every
row is provably independent of a null accessor read. What makes confining it to these four rows
safe is the narrower statement above — `null` in `scheduleInfo` means *absent* everywhere else in
the document, which is exactly the input the port represents as `None` and can evaluate.

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

Measured on the committed document: **18,356 rows**, each with a unique `id`, **1,050** of them
carrying a year-fraction `error` and 22 a `daysError` (the 1,072 `errRows` the summary reports),
**13,302,984 bytes**. The capture that produces it reports **147,239 checks** for this fixture —
more than any other, and the figure to compare a re-capture against; the full per-fixture reading
is the summary table in section 3.

Three of the Java tables contain literal duplicate rows carrying identical expectations — one in
`data_yearFraction`, one in `data_days` and nine date pairs repeated between the sections of
`data_ACTACTAFB` — and those rows are reproduced rather than de-duplicated, so the second
occurrence takes a `-2` id suffix; the total (11) is asserted, so a new duplicate cannot appear
unnoticed.

The capture asserts one hard anchor before emitting anything: `Act/Act ISDA` from 2011-12-28 to
2012-02-28 must equal `4/365 + 58/366` (`0.16942884946478032`). The `SIMPLE_30_360` /
`SIMPLE_30_360DAYS` markers of `DayCountTest` are **resolved** through `calc360` / `calc360Days`
exactly as the Java consumers resolve them, so no row carries `"NaN"` or `0` as a stand-in for a
30/360 expectation — and the fixture contains no non-finite year fraction at all.

**The checked-in script reproduces this fixture byte for byte**, at the uniform schema above: one
twelve-key row shape, a unique `id` on every row, `scheduleInfo` always an object, and all 18,356
rows. The hard anchor holds, and a re-capture that comes back `DIFFERS` is a finding — section 3's
verification record carries the comparison.

The document is 13 MB — large for a test resource, and what coverage of this size costs.
`DayCountParitySpec` reads it in one pass and decodes it in full, so size the forked test JVM's
heap with that in mind rather than assuming a small resource.

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
values instead; the input key set and its order are the same either way. That uniformity is what
lets the Scala decoder read every row's inputs into one case class. The *expectation* keys do vary
with the outcome, which is why the document holds four row shapes in all: 296 resolved rows (the
twelve inputs plus seven expectation keys), 572 rejected rows (the twelve plus `error`), and the
`data_replace` rows in their two forms (25 keys resolved, 14 rejected).

This is also the one fixture with **no `id`** and no distinct `source` — 879 rows over 16 `source`
values, 768 of them `grid.combinations` — so a row is identified by its `source` together with its
eleven input fields *and* the `replacedStartDate` of the `data_replace` rows, without which two of
them collide. The row-identity note at the top of this section has the measured counts.

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
describe — and it is part of a row's identity, because two of these rows share their base
definition and differ only in this date. `replacedDefinition` is the post-replacement definition,
rendered with the same eleven fields, which is what the Java test asserts field by field
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

Observed: 879 rows, 320 checks, 573 `error` rows.

#### `fx-baseline.json`

One row per scenario. **Every row carries these nine keys, in this order** — a query list a
scenario does not exercise is emitted as an empty array, never omitted:

`id`, `source`, `matrix`, `matrixState`, `queries`, `conversions`, `multi`, `crosses`, `merges`.

There is one variant, and it adds a tenth key rather than removing any: a row whose matrix
definition the **builder itself** rejects carries `"matrixState": null`, the five query lists
empty, and a top-level `error`. No row in the committed fixture exercises it — all 14 have exactly
the nine keys above — so a decoder that treats `error` as an optional tenth field reads both
shapes, and one that demands exactly nine keys reads today's fixture and breaks on the first
rejected definition anyone adds.

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

Observed: 14 rows, 100 checks, 14 `error` entries, 4 capture-only rows. Capture-time tolerances:
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
* `"captureOnly": true` (with `minorUnitDigits` where a currency's minor units are the point) — an
  entry with no hard-coded Java test constant to compare against: a seeded-random amount, or one of
  the per-currency sweeps below. **173 of this fixture's 355 expectation entries carry it.** The
  run's `capOnly` column reports a larger figure — 228 — because the 55 per-currency random-amount
  operations increment that counter without the field being emitted on the entry. The field is the
  contract a consumer reads; the column is a run-time tally. They are not the same number, and
  limitation 5 in section 3 records it. What the marker is for holds either way: an entry that
  *should* have been checked cannot hide among the unchecked ones.
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

Observed: 6 rows, 230 checks, 29 `error` entries and 355 expectation entries in total, of which
173 carry `captureOnly: true` (the run's `capOnly` tally reports 228, as above), covering all 74
configured and historic currencies.

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

**Observed: 3,846 rows, 50,561 checks, 2 `error` rows, 8.9 MB** (3,848 lines: the two brackets and
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

One flat row shape, the same keys on every row, with no optional field, no `null` and no `error`
entry anywhere. Eleven of the keys are the row's inputs — `id`, `a`, `b` (arrays), `scalar`,
`subArrayFrom`, `subArrayTo`, `matrixA`, `matrixB` (arrays of row arrays), `withRow`, `withColumn`,
`withValue` — and the remaining twenty-two are the expectations, each named for the operation that
produced it: `plusScalar`, `plusArray`, `minusScalar`, `minusArray`, `multipliedByScalar`,
`multipliedByArray`, `dividedByScalar`, `dividedByArray`, `mapSquared`, `reduceSum`, `sum`, `min`,
`max`, `sorted`, `concat`, `subArray`, `matrixMultipliedBy` (**scalar** — see section 8),
`matrixPlus`, `matrixMinus`, `matrixTranspose`, `matrixTotal`, `matrixWith`.

Every row's inputs are chosen so that all twenty-two operations have a value: the row carries the
`subArray` bounds and the `with` coordinates it is legal for, and the empty array and the
length-mismatched pair are not rows here. The behaviour those cases pin — `min`/`max` on an empty
array, `subArray` and `with` outside the array, the element-wise operations on arrays of different
lengths, and the matrix dimension preconditions — is asserted at capture time against the exception
types the Java tests name, rather than being serialised as an expectation.

Input population: 43 rows, each with a unique `id`, in three groups the capture asserts the size of
— 19 `javatest-*` rows derived from the literal arrays of `DoubleArrayTest` and `DoubleMatrixTest`,
18 `random-*` rows drawn from a generator seeded by this fixture alone and spanning the magnitudes
`1e-8` to `1e8` so both halves of the parity rule are exercised, and 6 `ieee-*` rows carrying signed
zero, infinities and `NaN` so the tagged-double policy is exercised end to end. A non-finite
expectation is compared by exact IEEE identity rather than by the `1e-9` tolerance. The fixture
contributes 382 capture-time checks against 24 capture-only entries, and the hard-coded constants of
`DoubleArrayTest` and `DoubleMatrixTest` are re-asserted at those tests' own `DELTA` of `1e-14`
alongside the rows.

### 7. Manifest schema, and the test-mapping document beside it

#### `reference-data-manifest.json`

`reference-data-manifest.json` is a single object, not a row array. It enumerates every reference
data table from the **Java** side with **every count asserted during capture**, so an edited
resource cannot silently reshape it. The document below is what a run produces —
254,640 bytes with exactly these 29 top-level keys, measured at the revision pinned in section 3 —
and it is committed, with `ReferenceDataManifestSpec` asserting that the ported Scala data objects
equal it: **all 29 top-level keys** are compared against a Scala data table, content *and* order,
and so is every nested shape of the four keys — `floatingRateNames`, `externalNames`,
`lenientPatterns`, `alternateNames` — whose content divides by family and whose shapes have separate
owners in the port. The spec's two `Pending…` declarations are **empty** at this revision and are
kept rather than deleted: its coverage test compares the union of covered and pending keys with the
keys the document actually carries, so a key a later capture adds is reported as uncovered instead
of going unchecked. The key names and the nesting below are therefore a contract on both sides —
renaming a key, flattening an object or turning an ordered array into a map breaks that spec, which
is exactly what it is for.

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
| `iborIndexConstants` | `{count, names[]}` | **113**, sorted by (constant name, field name) as every `*Constants.names` array is |
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

#### `java-test-mapping.csv` — a related document, not an output

The method-level traceability record beside the manifest, at
`strata-basics/src/test/resources/manifest/java-test-mapping.csv`. **This capture neither writes it
nor reads it.** Its format is documented here because this is where a reader of the manifest looks
for it, and because the format is part of what its own verifier checks.

It answers "which Scala test covers this Java test, and if none, why". Columns 3 to 5 are mapping
decisions no scanner can derive, so the document is authored and maintained alongside the Scala
test suite, and the test-scope row of `scripts/verify-gates.sh` is what verifies it — reconciling
every row against the Java test methods under `modules/**` and against the JUnit XML the Scala specs
produce.

It is a header line plus **1,876 data rows** of five fields, using RFC 4180's **field-quoting
rules** with **LF-only record separators** rather than that standard's CRLF, US-ASCII throughout,
and exactly one trailing newline: 315,909 bytes at this revision. A field is quoted only when it
contains a comma or a double quote, and a contained quote is doubled. The LF choice is deliberate —
it is this repository's convention for text files — so a strict RFC 4180 parser has to be configured
to accept LF records.

| Column | Contents |
|---|---|
| `java_test_class` | fully-qualified Java test class, for example `com.opengamma.strata.basics.date.DayCountTest` |
| `java_test_method` | the test method's name; an overloaded name carries its erased parameter types, `name(Type;Type)` |
| `scala_spec` | the fully-qualified Scala spec that covers it, or empty |
| `scala_test_name` | the test name inside that spec, or empty. It is frequently a sentence rather than the Java identifier, which is why this column cannot be derived |
| `status` | `ported`, `consolidated:<spec>`, `partial:<reason>` or `dropped:<reason>` |

The status taxonomy: `ported` names both a spec and a test name; `consolidated:<spec>` marks a Java
test absorbed by a spec that covers several, such as `JsonRoundTripSpec` taking every
`test_serialization`; `partial:<reason>` marks a member the port carries only where `strata-basics`
uses it, and occurs only for `GuavateTest` and `MapStreamTest`; `dropped:<reason>` marks a test of
machinery that is not ported at all. A `partial` or `dropped` row leaves both mapping columns empty,
and a `ported` or `consolidated` row fills both.


### 8. Notes for maintainers

Each of the following silently corrupts a baseline if forgotten. Most are verified properties of
the Java sources this capture reads; the first two and the last are properties of the capture's own
design. None is a style preference. Check them before changing how anything is captured.

* **Every committed document is regenerable, so a diff after a re-capture is a finding.** All
  seven — `daycount-baseline.json` included — come back byte for byte from a run of the checked-in
  script at the schemas sections 6 and 7 document. Re-capture freely, but compare document by
  document as step 3 describes, and treat any difference as something to investigate before
  committing rather than as the new baseline.

* **The last four lines of the script are the exit status, and each is load-bearing.** They read
  `if (CHECK.ok() && CAPTURE_COMPLETED) { System.setProperty("capture.exit.status", "0"); }`, then
  `/exit (Integer.getInteger("capture.exit.status", 1))`, then `/reset` and `/exit 1`. Three
  temptations each reintroduce a silent success, and all three were measured: computing the verdict
  in the exit expression (`/exit ((CHECK.ok() && CAPTURE_COMPLETED) ? 0 : 1)`) surrenders the status
  whenever the load destroys those declarations, because an `/exit` expression that fails to compile
  does not exit — JShell falls through to its REPL and terminates **zero**; `/exit`-ing from inside
  the preflight is impossible, since a JShell command cannot be conditional and `System.exit` in a
  snippet only kills the execution engine; and deleting the trailing `/reset` + `/exit 1` as dead
  code removes the only recovery from a javac internal crash, after which no exit expression of any
  shape — literal included — can be evaluated. Section 3 gives the mechanism and the nine-shape
  measurement. If you add a way for the capture to fail, make it a path that does *not* reach the
  `setProperty` statement, and add nothing to the exit expression.
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
* **Reflection gives completeness, never order — so never serialize the order it returns.**
  Enumerating a public constants holder with `getDeclaredFields()` is the only way to obtain a
  provably *complete* constant list, which is the whole point of a manifest, and the no-reflection
  requirement applies to the Scala **codec path**, not to a developer tool under `tools/`. Where an
  `ExtendedEnum` accessor gives the same answer, the accessor is preferred. What reflection does not
  give is an order: Java SE specifies none for the array `getDeclaredFields()` returns, and today's
  JVM happening to answer in declaration order is exactly the accident that would make a baseline
  irreproducible elsewhere. So the script never writes that order. `namedConstants` sorts every
  manifest constant array by (canonical name, field name) — the field name breaking the tie where a
  holder declares two constants for one value, as `OvernightIndices` does with the deprecated
  `EUR_ESTER` beside `EUR_ESTR` — and the day-count list is the declared
  `STANDARD_DAY_COUNT_NAMES` array, with reflection reduced to a completeness check whose two name
  lists are sorted before comparison. Keep it that way: if you add a constants group, sort it, and
  if you add a fixture population, declare its order. Limitation 4 of section 3 records what this
  buys and what is still only demonstrated.

For the wider context, `build.sbt` defines the module layout and the sbt commands, and
`strata-collect/README.md` describes the ported collect subset. The list of deliberate divergences
from the Java behaviour belongs in `SCALA_MIGRATION.md`, and the repository's
[root README](../../README.md) is where the Scala port is introduced; consult both for anything
this document treats as settled. Everything stated here about the script's behaviour and its output
is a statement about the revision whose hash section 3 pins, established by the runs that section
records; where a property is guaranteed by construction rather than observed, the text says which
of the two it is.
