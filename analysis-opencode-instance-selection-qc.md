# OpenCode instance-selection QC

## Bottom line

The prefix flow is present and ordered correctly: `C-u C-c o a` probes the
configured endpoints, prompts for an instance when multiple probes succeed,
then prompts for a workspace and agent.  The selected server URL is also used
for sends and compose-buffer identity, so equal numeric logIDs on two different
configured URLs do not collide.

Two stale-identity paths can still silently target the wrong workspace or
agent.  They should block merging because this command is a message router and
both failures can send user-authored text to a target other than the one shown
at selection time.

## Blocker

### 1. The chosen workspace identity is discarded before agent lookup

- **Location:** `src/rde/features/opencode.el:425`, `src/rde/features/opencode.el:468`, `src/rde/features/opencode.el:470`, `src/rde/features/opencode.el:472`
- `opencode--pick-workspace` returns a catalog row containing `projectID` and
  `workspaceID`, but `opencode--choose-agent` passes only its directory and
  display name to `opencode--pick-agent`.  The subsequent `/agent/logs` result
  has its own `project.projectID`, but `opencode--pick-agent` never compares it
  with the catalog row selected by the user.
- A stale catalog association, directory reassignment, duplicate historical
  workspace, or race can therefore make the selected directory resolve to a
  different loaded workspace.  Worse, the stale catalog name is forced into
  the agent prompt, so agents returned for project B can be displayed as being
  in project A.  A direct stub reproduces the accepted mismatch as
  `(:label "project-a / agent-b" :directory "/b")`.
- Preserve the selected `projectID`/`workspaceID` through agent lookup and bind
  the lookup to that identity.  If the API remains directory-based, at minimum
  compare the returned `projectID` and reject/reload on mismatch; an
  identity-addressed agent-list route is needed to fully distinguish
  workspaces.  Include identity fields in completion candidates so duplicate
  display labels cannot fall through `assoc` to the first row.

### 2. URL plus numeric logID is not a stable identity across daemon replacement

- **Location:** `src/rde/features/opencode.el:346`, `src/rde/features/opencode.el:503`, `src/rde/features/opencode.el:597`, `src/rde/features/opencode.el:684`
- The selection retains only endpoint URL, numeric logID, labels, and
  directory.  Later sends validate only that the URL looks non-empty/HTTP(S)
  and post immediately; they do not confirm that the endpoint is still a
  configured instance or that the log still has the selected creation/project
  identity.
- If a dev daemon is reset/replaced at the same local URL, numeric IDs can be
  reused by unrelated agents.  An old sticky selection then silently sends to
  the new holder of that number.  An old compose draft is even reused for a
  newly selected agent when URL and logID collide, leaking draft content across
  daemon generations.
- Store available immutable metadata from `/agent/logs` (at least `created`
  plus project/workspace identity), include it in compose-buffer identity, and
  revalidate it before sending.  Invalidate selections whose endpoint is no
  longer configured or whose fetched log metadata no longer matches; require a
  re-selection instead of relying on a 404/409, which cannot detect ID reuse.

## Should fix

### 1. Partial instance-probe failures are silently discarded

- **Location:** `src/rde/features/opencode.el:379`, `src/rde/features/opencode.el:403`, `src/rde/features/opencode.el:408`
- Failure details are shown only when every probe fails.  If one endpoint is
  offline, slow beyond the one-second timeout, malformed, or on an older API
  while another succeeds, the failed endpoint simply disappears.  When only
  one survives, the code also skips the instance prompt, leaving no indication
  why the requested configured instance was unavailable.
- Keep failed candidates non-selectable, but surface their names and reasons
  before/alongside the prompt.  Prefer a cheap liveness probe followed by a
  normal-timeout catalog fetch after selection, because a running daemon with a
  cold/large catalog can exceed one second and violate the requirement to offer
  running configured instances.  Test the timeout value, partial-failure
  diagnostic, malformed response, invalid config, and configured-order cases.

### 2. The new ERT tests are not part of the repository test target or CI

- **Location:** `tests/rde/opencode-tests.el:1`, `Makefile:33`, `.builds/main.yml:7`
- The eight ERT tests pass when invoked directly, but `make check` discovers
  only Scheme test modules.  SourceHut CI calls only `make check`, so none of
  the new Emacs Lisp tests run in the normal gate.
- Add the ERT batch invocation to the test target/CI and ensure the test file is
  tracked.  Run it on the declared minimum Emacs version (28.1), not only the
  locally available Emacs 30.2.

### 3. Tests do not cover the safety-critical prefix flow and stale cases

- **Location:** `tests/rde/opencode-tests.el:54`, `tests/rde/opencode-tests.el:74`, `tests/rde/opencode-tests.el:91`, `tests/rde/opencode-tests.el:106`
- The tests separately exercise instance prompting, one captured send URL,
  plain selection, and compose identity.  No test calls
  `opencode--choose-agent` with a prefix and proves the exact instance ->
  workspace -> agent order, selected-server propagation, and remembered
  selection.
- Add regression tests for catalog/agent project mismatch, same-URL logID reuse
  and old compose drafts, removal of a selected instance from configuration,
  stale local-layer fallback, duplicate workspace labels, partial probe
  failures, and the emitted RDE `opencode-server-instances` form.  Bind/reset
  `opencode-agent-alist` and `opencode-agent-current` per test so future tests
  cannot inherit the plain selection left by the current suite.

### 4. Configuration validation is inconsistent between RDE and selection paths

- **Location:** `src/rde/features/emacs-opencode.scm:88`, `src/rde/features/emacs-opencode.scm:90`, `src/rde/features/opencode.el:179`, `src/rde/features/opencode.el:476`
- The Scheme feature checks only that names and values are strings; it accepts
  empty names and malformed URLs.  Prefix discovery diagnoses these through
  `opencode--instance-candidates`, but plain selection bypasses that validation
  for the default URL and falls into a transport/parser error instead.
- Apply the same non-empty-name and HTTP(S) base-URL validation at RDE feature
  construction and before plain default-instance access.  `opencode--send`
  should also use `opencode--valid-server-url-p` rather than its weaker
  non-empty check, including compose-buffer sends.

### 5. A stale per-root selection masks a valid new global selection

- **Location:** `src/rde/features/opencode.el:503`, `src/rde/features/opencode.el:515`, `src/rde/features/opencode.el:520`
- Upgrading in a live Emacs retains the pre-0.3 `defvar` values.  If one root
  still has an old selection without `:server-url`, `opencode--selection-for-root`
  returns it before a valid global selection made from another root.  Validation
  then errors instead of dropping the invalid local entry and falling through
  to the valid global target or selection prompt.
- Validate each layer independently, evict invalid entries, and continue to the
  next layer.  Add an upgrade regression test around `opencode--agent-selection`;
  the current stale test exercises only `opencode--send` directly.

### 6. Instances with empty catalogs remain selectable dead ends

- **Location:** `src/rde/features/opencode.el:394`, `src/rde/features/opencode.el:411`, `src/rde/features/opencode.el:443`
- A valid empty array marks a daemon available.  Selecting it aborts at the
  workspace step even if another discovered daemon has usable workspaces; if it
  is the only responsive daemon, the instance prompt is skipped before the same
  failure.
- Show workspace counts and disable/filter empty instances, or return to the
  instance prompt after reporting the empty catalog.  Cover both single- and
  multi-instance empty-catalog behavior.

## Noted

- **Cross-instance numeric collision handling is correct:** selections capture
  normalized `:server-url`, sends use that captured URL, and compose buffers
  match both URL and logID (`src/rde/features/opencode.el:346`,
  `src/rde/features/opencode.el:605`, `src/rde/features/opencode.el:689`).
- **Core RDE wiring is present:** `#:server-instances` is validated as an alist,
  emitted into Emacs configuration, and configured in the keeper example
  (`src/rde/features/emacs-opencode.scm:64`,
  `src/rde/features/emacs-opencode.scm:116`,
  `examples/src/andrewzhurov/users/keeper.scm:453`).
- **URL deduplication is only textual:** removing trailing slashes does not
  canonicalize host case or default ports, so aliases of one daemon can appear
  as separate instances and separate compose identities
  (`src/rde/features/opencode.el:175`, `src/rde/features/opencode.el:352`).

## Verification

- `emacs --batch -Q -l tests/rde/opencode-tests.el -f ert-run-tests-batch-and-exit`:
  8/8 passed.
- `emacs --batch -Q --eval '(byte-compile-file "src/rde/features/opencode.el")'`:
  passed; the generated `.elc` was removed.
- `guix shell guile guile-gcrypt guile-git -- guild compile -L src -L tests src/rde/features/emacs-opencode.scm`:
  passed.
- `guix shell guile guile-gcrypt guile-git make -- make check`: 58 passed,
  7 expected failures, 0 failures; this command did not run the ERT file.

---

## Second pass - 2026-08-24 (after blocker fixes)

This section supersedes the earlier severity disposition for the current
uncommitted diff.  Both former Blockers are closed.  No new Blocker was found;
two former Should-fix items are closed and four remain partial.

### Former Blockers

#### 1. Closed - selected workspace identity is preserved and checked

- **Location:** `src/rde/features/opencode.el:309`,
  `src/rde/features/opencode.el:331`, `src/rde/features/opencode.el:350`,
  `src/rde/features/opencode.el:374`, `src/rde/features/opencode.el:551`
- The prefix flow now passes the complete selected catalog row and its catalog
  into `opencode--pick-agent`.  The directory lookup must match exactly one
  catalog row on `projectID`, `directory`, and `worktree`, and that row's
  `workspaceID` must equal the selected row's ID.  Stale, changed, and ambiguous
  mappings fail before the agent prompt is accepted.
- The resulting selection stores the checked `projectID`, `workspaceID`,
  directory, and worktree.  This is a fail-closed solution over the current
  directory-addressed `/agent/logs` API; an identity-addressed API would remove
  the extra catalog correlation but is not required for this fix.
- Regression coverage now includes prefix propagation and a project mismatch
  rejection (`tests/rde/opencode-tests.el:167`,
  `tests/rde/opencode-tests.el:196`).

#### 2. Closed - stale same-URL numeric logID reuse is guarded

- **Location:** `src/rde/features/opencode.el:390`,
  `src/rde/features/opencode.el:533`, `src/rde/features/opencode.el:690`,
  `src/rde/features/opencode.el:698`, `src/rde/features/opencode.el:738`,
  `src/rde/features/opencode.el:822`
- Selections now retain `created`, project, workspace, directory, and worktree
  identity in addition to endpoint and logID.  Before every POST,
  `opencode--validate-selection-target` refetches logs and the workspace catalog
  and requires the configured endpoint, logID/creation pair, project,
  workspace, directory, and worktree all to match.
- Compose reuse now keys on server URL, project ID, workspace ID, logID, and
  creation timestamp.  A replacement daemon reusing the same endpoint and
  numeric logID therefore gets a distinct draft; both one-shot and compose
  sends use the same revalidation path.
- Regression coverage now rejects a reused logID with a changed creation time
  and separates its compose buffer (`tests/rde/opencode-tests.el:136`,
  `tests/rde/opencode-tests.el:233`).  A daemon reset in the narrow interval
  between validation GETs and the POST remains an unavoidable client-side
  TOCTOU; eliminating it would require the POST API to accept/verify the
  stronger identity atomically.

### Former Should-fix Findings

#### 1. Partial - probe failures are visible, but discovery still probes the full catalog

- **Location:** `src/rde/features/opencode.el:115`,
  `src/rde/features/opencode.el:427`, `src/rde/features/opencode.el:464`
- The timeout increased from one to three seconds.  Partial failures are saved,
  emitted with names/reasons, and reflected as an unavailable count in the
  instance prompt; all-failure diagnostics remain aggregated.
- A running daemon can still be omitted when its full cold
  `/project/workspaces` catalog exceeds three seconds.  Discovery is sequential
  and still conflates catalog latency with daemon liveness.  A cheap liveness
  probe followed by a normal-timeout catalog fetch after instance choice would
  close the remainder.  The detailed reason is in the preceding message/
  `*Messages*`; the active completion prompt contains only the count.

#### 2. Partial - ERT is in CI, but minimum-version compatibility is not

- **Location:** `Makefile:33`, `Makefile:37`, `.builds/main.yml:7`
- `check` depends on `check-elisp`, which runs the ERT file through the pinned
  Emacs environment.  SourceHut already invokes `make check`, so the routing
  tests are no longer bypassed by CI.
- The pinned environment currently resolves Emacs 30.2, while package metadata
  declares Emacs 28.1.  The original CI omission is fixed, but compatibility at
  the declared minimum remains ungated.

#### 3. Partial - critical coverage improved, but important boundaries remain untested

- **Location:** `tests/rde/opencode-tests.el:112`,
  `tests/rde/opencode-tests.el:167`, `tests/rde/opencode-tests.el:196`,
  `tests/rde/opencode-tests.el:210`, `tests/rde/opencode-tests.el:233`
- Added coverage verifies prefix target propagation, project mismatch
  rejection, send-time creation mismatch, stale-local fallback, and compose
  separation on same-URL logID reuse.
- The prefix test mocks all three picker functions, so it does not prove actual
  completion prompt order.  There is no successful `opencode--pick-agent` test
  asserting that all identity fields come from verified responses, nor direct
  coverage for workspaceID/directory/worktree mismatch, partial-failure prompt
  visibility and timeout forwarding, empty catalogs, removal of a selected
  instance, malformed catalog entries, or generated RDE init serialization.
- Tests run on the pinned Emacs 30.2, while package metadata declares Emacs
  28.1 compatibility.  Minimum-version execution remains unverified.

#### 4. Partial - validation is applied on both sides but is not equivalent

- **Location:** `src/rde/features/emacs-opencode.scm:88`,
  `src/rde/features/emacs-opencode.scm:95`,
  `src/rde/features/opencode.el:185`, `src/rde/features/opencode.el:343`,
  `src/rde/features/opencode.el:524`
- The Scheme feature now validates default/additional HTTP(S) URLs and rejects
  empty instance names.  Plain selection validates its default URL, and
  selection/send validity requires the endpoint to remain configured.
- Scheme still accepts whitespace-only names, `"http:// "`, and URLs containing
  spaces such as `"http://bad url"`, while `opencode--instance-candidates`
  rejects them after generation.  Construction with
  `(("   " . "http:// "))` was reproduced successfully.  A non-string default
  URL in the plain flow also reaches `opencode--normalize-server-url` before
  `opencode--pick-agent` can issue its user-facing validation error, producing a
  raw `wrong-type-argument` instead.  Validation and trimming should be shared
  semantically, and Scheme-side rejection/serialization tests should cover it.

#### 5. Closed - stale local selections fall through to a valid global selection

- **Location:** `src/rde/features/opencode.el:592`,
  `tests/rde/opencode-tests.el:210`
- Per-root and global layers are now validated independently.  An invalid old
  local entry is deleted before the valid global selection is considered, and
  the upgrade behavior has a regression test.

#### 6. Closed - empty catalogs are no longer selectable dead ends

- **Location:** `src/rde/features/opencode.el:442`,
  `src/rde/features/opencode.el:449`
- Discovery filters unusable workspace rows and records an instance with no
  usable workspaces as unavailable.  Other usable instances remain selectable;
  if none remain, the aggregate error reports the empty-catalog reason.
- Direct ERT coverage for the single- and multi-instance empty cases is still
  desirable and is counted under the remaining test-gap item.

### New Non-blocking Regressions

- **Catalog availability now gates plain selection and every send:**
  `opencode--pick-agent` and `opencode--validate-selection-target` synchronously
  require `/project/workspaces` in addition to `/agent/logs`
  (`src/rde/features/opencode.el:350`,
  `src/rde/features/opencode.el:705`).  A transient catalog failure or slowdown
  blocks an otherwise valid target.  This is fail-closed rather than a
  misrouting risk, but it widens the availability dependency and should receive
  an explicit diagnostic/retry policy.
- **Malformed default URL UX:** the plain path normalizes
  `opencode-server-url` before the new picker-level validator
  (`src/rde/features/opencode.el:565`).  Non-string customization values expose
  an internal type error rather than the intended `user-error`; validate before
  normalization.

### Second-pass Verification

- `emacs --batch -Q -l tests/rde/opencode-tests.el -f ert-run-tests-batch-and-exit`:
  13/13 passed.
- `make check-elisp` through the pinned time-machine Emacs 30.2: 13/13 passed.
- Integrated `make EMACS=emacs check` in a Guix shell: ERT 13/13 passed;
  Scheme suite 58 passed, 7 expected failures, 0 failures.
- Emacs Lisp byte compilation, Scheme feature compilation, valid feature
  construction, and `git diff --check` passed.  The generated `.elc` was
  removed.

---

## Third pass - 2026-08-24 (flattened agent flow)

The simplified prefix flow is implemented as instance -> one flattened agent
list.  No workspace completion prompt remains in code, documentation, Scheme
integration, or the keeper example.  Internal workspace resolution still
fails closed on project/directory/worktree ambiguity and preserves
`workspaceID` in each candidate for send-time and compose identity checks.

### Should fix - flattened labels can still be ambiguous

- **Location:** `src/rde/features/opencode.el:358`,
  `src/rde/features/opencode.el:378`, `src/rde/features/opencode.el:390`,
  `src/rde/features/opencode.el:412`
- Flattening iterates every catalog workspace, validates each directory lookup
  against the selected catalog row, and builds full selection plists before the
  single `Agent on ...` prompt.  Labels include project name, agent title, and
  logID, while the hidden selection retains endpoint, creation time, project,
  workspace, directory, and worktree identity.
- Two workspaces of the same project can nevertheless produce display choices
  such as `Project / agent (#1)` and `Project / agent (#2)` with no directory,
  branch, or workspace hint.  Numeric logIDs distinguish the strings, so
  `assoc` routing remains deterministic, but the user may not know which
  workspace/project realization each agent belongs to.  Include an abbreviated
  directory, branch, or workspace name/ID in the flattened candidate label.

### Should fix - one stale workspace aborts the entire flattened list

- **Location:** `src/rde/features/opencode.el:360`,
  `src/rde/features/opencode.el:418`
- `opencode--pick-instance-agent` calls `opencode--agent-selections` for every
  workspace without isolating errors.  If any one catalog row is stale,
  ambiguous, no longer loaded, or its `/agent/logs` request fails, the command
  aborts before showing agents collected from other valid workspaces.
- This is fail-closed and cannot misroute, but it makes the flattened flow
  brittle.  Isolate per-workspace failures, offer agents from verified rows,
  and report skipped workspace diagnostics; fail only when no verified agents
  remain.

### Test status

- `tests/rde/opencode-tests.el:173` verifies prefix dispatch calls instance then
  the flattened agent picker, and `tests/rde/opencode-tests.el:198` verifies
  agents from two workspaces reach one selection list.
- The flattening test stubs candidate construction and does not assert actual
  project/workspace labels or identity fields.  Add coverage for same-project
  multi-workspace disambiguation, one stale workspace alongside one valid
  workspace, and explicit proof that the prefix path invokes only the instance
  and agent completion prompts.
- Focused ERT and pinned `make check-elisp` both passed 15/15; byte compilation
  and `git diff --check` passed.  The generated `.elc` was removed.
