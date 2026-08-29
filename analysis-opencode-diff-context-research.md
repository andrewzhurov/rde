# OpenCode Diff Context Research

## Scope and evidence

This report examines the current OpenCode Emacs client and the locally installed
Magit 4.5.0 and Majutsu 0.6.0-1.2ab2adb sources.  The RDE checkout configures
both packages but has no existing OpenCode-to-diff integration.

* Current generic capture is `opencode--context-preamble` in
  `src/rde/features/opencode.el:684`.  It reports the display buffer's
  `buffer-file-name`/name, Emacs buffer line numbers, and literal region text;
  a Magit/Majutsu diff buffer therefore currently yields non-semantic display
  coordinates.
* RDE enables Majutsu through `feature-jujutsu` in
  `src/rde/features/jujutsu.scm:32`, autoloads `majutsu`, and binds it to
  `C-x j` (`:109-114`).  Its Git feature configures Magit but adds no diff
  context hooks: `src/rde/features/emacs-xyz.scm:3495-3643`.
* Magit source inspected:
  `/gnu/store/i1i1z0iccv66cny30xir5lb91q0n8bcf-emacs-magit-4.5.0/share/emacs/site-lisp/magit-4.5.0/`.
* Majutsu source inspected:
  `/gnu/store/l7fdpdmgqn9555qnz3xy1al1z6g826wk-emacs-majutsu-0.6.0-1.2ab2adb/share/emacs/site-lisp/majutsu-0.6.0-1.2ab2adb/`.

## Shared section substrate

Magit and Majutsu both use `magit-section-mode` and the same EIEIO section
tree, not per-line semantic text properties.  `magit-section-at` returns the
object stored in the `magit-section` text property at point
(`magit-section.el:561-563`); `magit-current-section` additionally handles
context-menu point and falls back to `magit-root-section` (`:552-559`).
Each section has public slots including `type`, `value`, `start`, `content`,
`end`, `parent`, and `children` (`:416-431`).  `magit-section-lineage` and
`magit-section-ident` are useful stable traversal/identity helpers
(`:565-632`).  Use `magit-section-match`, `magit-section-value-if`, and
`magit-section-parent-value` rather than assuming a fixed nesting depth.

This is the strongest common semantic hook: from point and each selected
line/range endpoint, obtain its section and climb to file and hunk ancestors.
The ordinary region is still display text, so it must be separately mapped to
source line coordinates; do not treat `line-number-at-pos` in the diff buffer
as a file line number.

## Magit

### Modes and repository/revision context

* Relevant modes are `magit-diff-mode`, `magit-revision-mode` (a committed
  revision display; derives from the former), `magit-status-mode`, and
  `magit-log-mode` sections containing diffs.  `magit-revision-mode` is
  declared at `magit-diff.el:2705-2728`; matching `derived-mode-p
  'magit-diff-mode` covers the first two but not every status/log embedded
  diff.
* The reliable repository root is `(magit-toplevel)` while a Magit buffer is
  current; its `default-directory` is also repository-oriented.  For a
  package-neutral fallback, use the client’s existing `(vc-root-dir)` path
  resolution (`opencode.el:162-174`).
* Buffer-local Magit context includes `magit-buffer-diff-type`,
  `magit-buffer-diff-args`, `magit-buffer-diff-files`, `magit-buffer-range`,
  `magit-buffer-revision`, `magit-buffer-revision-hash`,
  `magit-buffer-revisions`, and `magit-buffer-typearg`
  (`magit-mode.el:582-596`).  In a revision buffer, `magit-buffer-revision`
  is the displayed commit.  A diff buffer instead commonly has a range or a
  diff type (`committed`, `staged`, `unstaged`).  These variables are useful
  package context but are buffer-state implementation interface, not a
  complete semantic API for every buffer shape.
* `magit-diff-visit-file` documents Magit’s side rule: added/context lines are
  new/right/"our" and removed lines are old/left/"their"
  (`magit-diff.el:1615-1655`).  Its internal side resolver maps a committed
  commit to `REV^` and `REV`, a range to its endpoints, staged to `HEAD` and
  index, and unstaged to index/HEAD and worktree (`:1768-1794`).  This is
  excellent evidence of intended semantics, but `magit-diff-visit--sides` is
  internal and should not be called by the client.

### File, hunk, side, and selected lines

* File sections have type `file`, section `value` equal to the new path, and
  `source` equal to the old path when it differs.  They also retain raw header
  text and a binary flag.  `magit-diff-insert-file-section` constructs exactly
  this object (`magit-diff.el:2527-2548`); parsing recognizes rename and
  `---`/`+++` old/new paths (`:2467-2525`).  Read with `(oref file value)` and
  `(oref file source)` after checking a `file` ancestor.
* Hunk sections have type `hunk`.  Their `from-range` and `to-range` slots are
  `(START COUNT)` lists, with `combined`, `about`, and `value` also available.
  Magit builds them while washing the unified header at `:2659-2687`.
* Determine a normal unified diff line’s side from its first character:
  `-` is old-only, `+` is new-only, and space is context/both.  For a combined
  hunk, use its `combined` metadata and do not apply the one-character rule;
  Magit’s own mapping accounts for multiple prefix columns
  (`magit-diff-hunk-line:1811-1839`).
* For a point or a contiguous region that stays within one hunk, calculate
  source coordinates from the hunk ranges and scan hunk body lines through the
  selected display positions.  Increment old coordinate for `-` and context,
  new coordinate for `+` and context; a `-` has no new line and `+` has no old
  line.  Region endpoints crossing hunks/files should be represented as
  separate fragments, not collapsed to one path/range.  The public command
  `magit-diff-visit-file` can validate the conceptual mapping interactively;
  `magit-diff-hunk-line` is an implementation helper rather than a stable
  client dependency.

## Majutsu

### Modes, repository, and revisions

* `majutsu-mode` derives from `magit-section-mode`
  (`majutsu-mode.el:466-470`) and `majutsu-diff-mode` derives from it
  (`majutsu-diff.el:1317-1325`).  Majutsu log buffers (`majutsu-log-mode`) can
  contain `jj-commit` sections; direct diff buffers use `majutsu-diff-mode`.
* Majutsu sets both `default-directory` and the buffer-local
  `majutsu--default-directory` to its top-level directory during setup
  (`majutsu-mode.el:288-323`; variable at `:178`).  Use this/default-directory
  as root, falling back to `vc-root-dir` if not a Majutsu buffer.
* Its supported displayed-range record is `majutsu-buffer-diff-range`, with
  formatting args and fileset filters alongside it
  (`majutsu-mode.el:481-493`).  `majutsu-diff--revisions` interprets it as
  old/left and new/right: `-r REV`/`--revisions=REV` becomes `REV-` -> `REV`,
  explicit `--from`/`--to` defaults missing sides to `@-`/`@`, and no range is
  `@-` -> `@` (`majutsu-diff.el:1101-1120`).
* `jj-commit` values in a log are revsets, accessible through the shared
  `magit-section-value-if`; Majutsu’s own DWIM diff does this at
  `majutsu-diff.el:1381-1385`.  Treat them as jj revsets rather than assuming
  immutable Git-style hashes.  A feedback packet for agent output should keep
  both the exact input/range arguments and any selected `jj-commit` value.

### File, hunk, side, and selected lines

* Majutsu registers `jj-file` and `jj-hunk` custom section classes on Magit’s
  section type map (`majutsu-base.el:106-138`).  `jj-file` has `value` equal
  to the displayed path and retains its complete raw Git-style header in the
  `header` slot (`majutsu-diff.el:591-621`).  Unlike Magit’s `file` section,
  it does not expose a separately parsed old path.  Derive old/new paths from
  the raw `diff --git`/extended headers only when needed; otherwise report the
  canonical displayed path as the selected file.
* A `jj-hunk` has `value` `(FILE . FROM-RANGE)`, `from-range`, optional
  `from-ranges` for combined hunks, `to-range`, `about`, and `combined`.
  Majutsu parses the unified header and creates these slots at
  `majutsu-diff.el:624-672`.
* Normal git-backend side mapping is `-` -> old/left, `+` -> new/right, and
  context -> both.  `majutsu-diff-visit-file` explicitly documents this
  (`:1184-1239`) and uses the hunk mapper at `:1129-1158`.
* The optional `--color-words` backend is a material exception: visual lines
  may not preserve normal unified-diff geometry.  Majutsu provides
  `majutsu-color-words-line-info-at-point` and
  `majutsu-color-words-side-at-point` (declared at `majutsu-diff.el:36-44`),
  and its visitor uses their `:from-line`/`:to-line` facts (`:1200-1224`).
  Those are package-internal helpers, so a minimal integration should either
  use them guarded by `fboundp` and accept version coupling, or label the
  selection as rendered diff text rather than claiming exact source lines.

## Public/stable versus internal details

Use the shared, documented Magit-Section entry points and object slots as the
main dependency: `derived-mode-p`, `magit-current-section`,
`magit-section-at`, `magit-section-match`, `magit-section-value-if`,
`magit-section-parent-value`, `magit-section-lineage`, `magit-section-ident`,
and `magit-region-values`.  `magit-diff-visit-file` and
`majutsu-diff-visit-file` are public interactive commands and establish their
side behavior, but they navigate rather than return a context record.

The following are useful evidence and may be guarded optional enhancements,
`magit-diff--file-section`, `magit-diff--hunk-section`,
`magit-diff--dwim`, `magit-diff-visit--sides`, and
`magit-diff-hunk-line`; Majutsu `majutsu-diff--revisions`,
`majutsu-diff--hunk-line`, `majutsu-diff--on-removed-line-p`, and all
`majutsu-diff--color-words-*` helpers.  Leading `--` follows the conventional
internal naming boundary.  Buffer-local `magit-buffer-*` and
`majutsu-buffer-*` variables are maintained package state and are less stable
than a deliberately exported accessor, but are currently the only direct
record of displayed ranges.

## Minimal reliable packet for feedback on an agent-made commit diff

For a selected change in a Magit revision/diff or Majutsu diff, capture one
packet per file/hunk fragment:

* VCS kind (`git` or `jj`) and canonical repository/workspace root.
* Exact displayed revision identity: Git commit hash when known, otherwise the
  raw Magit range/type/revision variables; jj raw diff-range arguments and any
  selected `jj-commit` revset.  Do not infer that the OpenCode agent’s log ID
  is a VCS commit identifier.
* Old and new revision expressions when directly known, retaining symbolic
  values such as `{index}`, `{worktree}`, `@-`, and `@` rather than fabricating
  hashes.
* New and old relative paths (old optional), file status/rename indication if
  available.
* Hunk old/new `(start,count)` ranges, whether combined, and exact unified
  hunk header.
* Selected source fragments: side (`old`, `new`, or `context`), exact source
  line number/range for each applicable side, and unpropertized rendered diff
  text.  Include display selection bounds only as diagnostic provenance.

This is sufficient for a request such as “review the agent-made commit at this
hunk/these changed lines” while preserving both sides, renames, and uncommitted
states.  Do not send only copied selected text: removals are absent from the
new file and added lines have no old-file coordinate.

## Arbitrary `diff-mode` fallback

For buffers that are not Magit/Majutsu section buffers, use no package
assumptions.  First retain the existing generic buffer/file/region preamble.
If `derived-mode-p 'diff-mode` or text resembles a unified diff, parse only
the visible unified syntax: `diff --git`, `---`, `+++`, and `@@ -a,b +c,d @@`.
Track current old/new path and hunk ranges while scanning each selected line;
apply prefix-based old/new counters as above.  Preserve unmatched headers and
ambiguous/non-unified output as literal diff text with `source-location:
unknown`, rather than emitting false file coordinates.  This fallback cannot
reliably identify repository root, revision pair, or rename semantics beyond
the text shown; use `vc-root-dir`/`default-directory` for a best-effort root
and omit unavailable facts.

## Recommendation boundary

Implement detection as an optional semantic-context extractor before generic
capture: section-based Magit first, then Majutsu (which shares sections but has
`jj-file`/`jj-hunk` and jj range rules), then a conservative unified-diff
parser, finally the current generic path.  The extractor should return facts
and confidence/fallback markers; message wording and final product semantics
remain separate decisions.
