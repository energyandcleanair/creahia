# Agent Instructions

## Project purpose

- This repository is an R package for air-pollution health impact assessment
  (HIA).
- Changes can affect both software behavior and scientific interpretation.
  Numerical reproduction is not the same as scientific approval.

## Start every task here

1. Check the active worktree, branch, `git status`, and recent commit before
   editing. Do not overwrite or reformat unrelated user changes.
2. Read `plans/README.md` and follow its task-specific reading route.
3. Inspect the current code and tests before relying on planning prose. When
   they disagree, stop and report the mismatch instead of guessing.
4. Define a small, reviewable change and its validation before implementation.

Do not assume that a branch name, rollout date, or unfinished proposal in a
historical planning file still describes the current repository.

## Architecture invariants

- Preserve the top-level compute flow unless an explicitly approved change
  requires otherwise:
  `compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()`.
- The Registry stores structured CRF definitions and provenance. References
  identify scientific sources. Presets provide reusable selections, and Sets
  represent explicit analysis configurations.
- The compatibility adapter translates supported legacy-shaped calls into the
  canonical Registry-driven configuration. It must not introduce a second HIA
  formula or silently change established results.
- Keep CRF profile selection, RR source selection, EPI version, and population
  year as separate decisions. Do not infer one from another.
- Historical reproduction data and test fixtures are not automatically
  approved scientific defaults and must not appear in user-facing discovery
  unless they have been reviewed for that purpose.
- Aggregate and component outcomes that represent overlapping burden must not
  be combined without an explicit double-counting review.

## Change and review boundaries

- Routine implementation fixes may proceed when their intended behavior is
  already established and focused tests can prove that behavior.
- Stop and request human review before accepting changes to Registry, RR, CRF,
  or EPI scientific content; methodology assumptions; public APIs; or the
  top-level compute flow.
- A report-blocking failure, package installation failure, or error in an
  established workflow may be handled as urgent software maintenance. Urgency
  does not authorize changing a data source, disease definition, model
  assumption, or other scientific choice.
- Preserve current public behavior unless the task explicitly authorizes a
  breaking change. Do not perform broad refactors as a side effect of a bug
  fix.
- Implement changes in small slices. Explain which files and functions are
  affected, why the change is needed, and how it will be checked.

## Numerical and fingerprint safety

- Use both live parity comparisons and fixed fingerprints where numerical
  behavior is at risk. Live parity checks whether two routes agree today;
  fixed fingerprints can detect when both routes drift together.
- Never update a fingerprint, snapshot, tolerance, or authorised-difference
  rule only to make a failing test pass.
- Before accepting a numerical change, identify its cause, affected versions
  and configurations, expected rows or values, scientific meaning, and review
  owner. Unexplained differences remain failures.
- Keep current-route equivalence separate from authorised differences between
  historical package releases.
- Treat `tests/testthat/test_results_immutability.R` and its helper files as
  release safeguards, not fixture-generation utilities.

## Validation expectations

- Run the smallest relevant tests first. For routing or numerical changes,
  include the applicable compatibility, parity, fixed-fingerprint, and output-
  structure checks.
- Run the broader development suite when shared computation, selection,
  Registry compilation, or compatibility behavior changes.
- The current full development suite includes a cross-version test that may
  install old releases and write fingerprint files. Follow the runbook's
  side-effect checks before running it.
- Run an installed-package check when package contents, dependencies, build
  exclusions, or installed-file lookup may be affected.
- Report development-test results, installed-package results, environment-only
  warnings, and scientific review as separate forms of evidence.
- Do not describe a successful test run as methodology approval or release
  approval.

## Documentation roles

- `plans/` is the tracked, current maintenance and architecture entry point.
- `.github/prompts/dev/rr_crf_redesign/` contains private planning history and
  detailed development records. Do not require maintainers to read it all, and
  do not copy it wholesale into `plans/`.
- `doc/` contains user-facing or HIA-team-facing documentation.
- Update the relevant shared plan when a change alters architecture,
  maintenance procedures, safety boundaries, or validation commands.
- Keep temporary checklists, personal collaboration preferences, and dated
  branch instructions out of this file.
