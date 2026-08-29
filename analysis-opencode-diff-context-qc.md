# RDE OpenCode Semantic Diff Context QC

Final current-snapshot review against the research brief and installed Magit
4.5.0 and Majutsu 0.6.0 sources.  No implementation files were changed.

## Blockers

None found.

## Should Fix

None required for the agreed contract.

Optional test coverage remains at the top-level extractor boundary.  The suite
thoroughly exercises the semantic helpers, but does not yet call
`opencode--semantic-diff-context`, `opencode--region-sections`, or
`opencode--capture-context` with a complete section-property buffer.  Future
regressions could cover both point/mark orientations, whole-status and
header-crossing rejection, complete partial-line/final-newline rendering,
combined/color-words rejection, and production all-entry jj parsing.  This is a
coverage improvement, not a known correctness defect.

## Noted

- Semantic detection now distinguishes rendered diff files from ordinary Magit
  status-list `file` sections using header, binary, or hunk-child evidence.  A
  live untracked-file selection with lineage `(file untracked status)` returns
  no semantic capture and therefore preserves generic context behavior.
- Region inspection scans every intersecting `magit-section` property span, so
  selections entering/enclosing actual diff headers, files, or multiple hunks
  reject instead of falling through to generic display coordinates.
- Ordinary unified selections preserve exact selected source text after the
  marker, trailing spaces, partial-line bounds, and final-line-break selection.
  Non-source metadata, combined hunks, and Majutsu color-words selections reject.
- Compact old/new/sign/text gutters are deterministic.  Removed, added, and
  context signs are configurable with defaults `-`, `+`, and space.
- Git staged, unstaged, ranges, revisions including root commits, embedded logs,
  `-R`, and all three stash subdiffs retain complete distinct side keys.
  `--no-index` rejects rather than claiming repository revision semantics.
- jj revision sides retain every resolved full commit/change identity.  Revision
  arguments are unioned and total boundaries use `roots(UNION)- -> heads(UNION)`;
  default/single merge revisions retain all parent identities.  Explicit
  `--from`/`--to` parsing is local.
- Rename paths distinguish old/new locations.  Host detection is limited to
  supported Magit modes and direct Majutsu diff mode.  Compose dedup compares
  only the immediately previous structured capture.
- Against installed Magit 4.5 and Majutsu 0.6 load paths, strict byte compilation
  and `git diff --check` pass.  The current ERT suite passes 29/29 with
  `TMPDIR=/dev/shm`; `/tmp` remains full.
