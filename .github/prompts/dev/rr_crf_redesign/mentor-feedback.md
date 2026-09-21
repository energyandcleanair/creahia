# Mentor Feedback

Use this file as the inbox for weekly feedback from senior colleagues. After new
feedback is added, sort it into:

- `00-roadmap.md` for direction changes
- a step file for implementation changes
- `decision-log.md` for confirmed technical decisions

## Template

### YYYY-MM-DD

Feedback:

-

Impact:

- Roadmap:
- Step file:
- Decision log:
- Code/tests:

Follow-up questions:

-

## 2026-06-01

Feedback:

- Rename concentration-change variables so they clearly represent deltas rather
  than absolute concentration points.
- Use constants for CRF form values instead of hard-coded strings.
- Remove old compatibility translation for `C_causes`, `GBD_causes`,
  `RR_sources`, and `CRF_version` in the breaking-change version.
- Remove old `_NRT` pollutant-name parsing.
- Remove `.region` branching and default to the GEMM data variant that includes
  China.
- Return a list from the main reporting-facing function, including both results
  and a references table.
- Strengthen CRF registry validation for `form` values.
- Move shared compile/upsert helpers to `R/compile_crfs_utils.R`.
- Expand the GEMM compiler to process the full GEMM dataset, then later extend
  the compiler pattern to Fusion and GBD.
- Keep the age interval `NA` bug unfixed for now to avoid widening the registry
  migration.

Impact:

- Roadmap: reprioritize naming/constants, compiler utils, full GEMM automation,
  and legacy cleanup before the public output-shape change.
- Step file: update registry validation, tabular migration, and log-linear
  migration notes; add separate planned steps for legacy cleanup/output and full
  compiler automation.
- Decision log: record constants, concentration-delta naming, compatibility
  removal, NRT/region cleanup, references output, and deferred age bug.
- Code/tests: no code change yet; future implementation should add focused tests
  for each item before replacing legacy paths.

Resolved decisions:

- Keep `log_linear` as the canonical stored value; do not migrate it to
  `log-linear`.
- All RR/CRF redesign development belongs on `feat/unify_rr_and_crf`.
- Development planning files live under `.github/prompts/dev/rr_crf_redesign/`.

Follow-up questions:

- Should the references table be returned by `compute_hia()` directly, or by a
  higher-level wrapper that prepares report-ready output?

## 2026-06-15

Feedback:

- Do not work on EPI data generation yet.
- Do not use `dc_groups.csv` for EPI validation yet.
- Keep `dc_groups.csv` scoped to double-counting rules only.
- Remove `pollutant` and `outcome` from `dc_groups.csv` so the file describes
  cause relationships rather than expanded pollutant/outcome combinations.
- Double-counting rules are cause relationships, but they must still be applied
  within each `pollutant` + `outcome` group.
- Keep the legacy `add_double_counted()` behavior for now: mark
  `double_counted`, but do not prevent users from selecting CRFs.
- Reimplement `add_double_counted()` with `dc_groups.csv` instead of the
  hard-coded `CAUSE_NCDLRI_INCLUDED` and `CAUSE_CV_INCLUDED` paths.
- Do not add a default selection-time hard error for double counting. Keep the
  check optional.
- Rename the bundle concept to preset.
- Store presets under `inst/extdata/crf/presets/`, for example
  `experimental_default.csv`, instead of hard-coding them in R code.

Impact:

- Roadmap: mark the double-counting rules and preset-based CRF selection slice
  as completed; keep compute-path integration and output-shape changes as later
  work.
- Step file: no EPI generation or EPI validation step should be expanded from
  this feedback. The next code-facing work belongs to the selector/compute
  integration path, not EPI data generation.
- Decision log: record that `dc_groups.csv` is double-counting-only,
  cause-level, and applied inside pollutant/outcome groups; record preset-based
  CRF selection and optional selection-time double-counting checks.
- Code/tests: completed in small slices with focused tests for
  `crf_double_counting`, `compute_hia_impacts`, and `crf_select`.

Completed checklist:

- [x] Confirmed active work on `feat/unify_rr_and_crf`.
- [x] Rebased this feature branch on the available `origin/master` state before
  the implementation slice.
- [x] Refactored `inst/extdata/crf/dc_groups.csv` to
  `dc_group,cause,role,notes`.
- [x] Removed `pollutant` and `outcome` from `dc_groups.csv`.
- [x] Kept only true double-counting cause groups:
  `ncdlri_mortality` and `cv_mortality`.
- [x] Removed the old `member` role and the `LRI.child` age-partition rule from
  `dc_groups.csv`.
- [x] Updated `R/crf_double_counting.R` validation for the cause-level schema.
- [x] Updated double-counting conflict detection so it joins rules by `cause`
  and evaluates conflicts within `pollutant` + `outcome` groups.
- [x] Updated `add_double_counted()` to infer component double-counting from
  `dc_groups.csv` while preserving explicit `double_counted` values from CRF
  metadata.
- [x] Preserved legacy `add_double_counted()` behavior: it marks
  `double_counted` rows and does not block CRF selection.
- [x] Added `inst/extdata/crf/presets/experimental_default.csv`.
- [x] Replaced hard-coded bundle specs with file-backed CRF presets.
- [x] Removed the temporary `crfs_bundle()` API instead of keeping an alias.
- [x] Renamed `select_crfs()` to `resolve_crf_selection()` to reflect that the
  function resolves a selection table into registry rows.
- [x] Added packaged-preset tests so future preset CSV files must load, have the
  required columns, and resolve to registry rows.
- [x] Changed selection-time double-counting checks to be optional rather than
  enabled by default.

Validation completed:

- `devtools::test(filter = "crf_double_counting")` passed.
- `devtools::test(filter = "compute_hia_impacts")` passed.
- `devtools::test(filter = "crf_select")` passed after the preset and
  `resolve_crf_selection()` updates.

Remaining follow-up:

- Run `devtools::test(filter = "crf")` after the documentation updates and any
  final selector cleanup.
- Continue with registry-to-compute integration only after the selector contract
  is stable.
- Do not start EPI validation or EPI generation from this feedback.

## 2026-06-15 Follow-up

Feedback:

- Registry-vs-legacy PAF tests should assert more than equality. Add checks that
  the compared result is not `NA` and not all zero.
- Investigate why the new registry/tabular path passes `epi_version` explicitly
  while the legacy path appears to work without doing so.
- Treat the `epi_version` discrepancy as technical debt to trace before deeper
  compute-path replacement.
- Presets are useful, but a higher-level Sets interface may be needed for user
  experience.
- Sets should behave like a "set menu": choose a base model family, then override
  selected causes/outcomes with specific external papers.
- Avoid making users manually copy long CRF IDs into normal workflows.
- Keep scientific transparency clear when a broad preset/set name mixes sources,
  such as a GBD base plus asthma or birth-outcome papers.

Impact:

- Roadmap: add parity-test hardening and `epi_version` tracing before deeper
  compute integration; add Sets as a design track after presets.
- Step file: add `06-parity-testing-and-epi-version.md` and
  `07-presets-and-sets-architecture.md`.
- Decision log: record the not-zero/not-`NA` testing rule, `epi_version`
  technical debt, and Sets-over-Presets design direction.
- Code/tests: no code change in this documentation update; next implementation
  should start with parity-test guards, then trace `epi_version`.

Resolved decisions:

- Presets remain the current concrete file-backed selection layer.
- Sets are a design concept for later, not the next implementation slice.
- Equality-only parity tests are insufficient for HIA result comparison.

Follow-up questions:

- Should the not-zero guard require any PAF value to be non-zero, or should it
  require a specific estimate column such as `central` to be non-zero?
- Should `epi_version` be explicit in every registry-facing function, or only at
  the public compute boundary with defaults passed downward?

## 2026-06-29 Preset/Set Architecture Feedback

Feedback:

- Current workflows can be confusing because log-linear CRFs are always added
  even when the main wrapper argument says `gbd2023` or `fusion`.
- This gives the impression that all outcomes are computed from GBD 2023 or
  Fusion RR curves, even though some outcomes come from unrelated log-linear
  CRFs.
- The old CRF table combines response functions from different references and
  different outcome families, including asthma, work absences, and birth-related
  outcomes.
- Create one or several explicit presets for these log-linear/original CRFs.
- If using one preset, possible names include `original_crfs` or
  `crfs_original`.
- If using several presets, split either by reference/paper or by outcome family,
  for example `asthma_original`, `birth_original`, or similar names.
- The Set creation function should be able to combine several presets.
- Consider whether `crfs_set()` should use `add=`, `remove=`, and `replace=`
  instead of one `overrides=` argument.

Impact:

- Roadmap: keep Preset/Set as a design track, but revise the Set concept from
  "one base preset plus overrides" to "combine several presets, then optionally
  add/remove/replace selected CRFs".
- Step file: update `07-presets-and-sets-architecture.md` to make source/model
  presets scientifically precise and to keep original/log-linear CRFs in explicit
  presets.
- Decision log: record this as a revised direction pending final confirmation,
  not as implemented behavior.
- Code/tests: no code change yet.

Resolved direction:

- Source/model presets such as `gbd2023` and `fusion` should not silently include
  unrelated original/log-linear CRFs.
- Original/log-linear CRFs should live in one or more explicit presets.
- Sets should support combining several presets.

Open questions:

- Should original/log-linear CRFs be grouped as one preset or split by
  reference/outcome family?
- Should the final Set API use `presets=`, `add=`, `remove=`, and `replace=`?
- What names should be used for original/log-linear CRF presets?

## 2026-08-17: Historical CRF-profile feedback

Source:

- Comments on the asynchronous "CRF Registry Redesign and Historical Workflow
  Profiles" brief. The exported comments are retained outside the repository;
  the decisions below are the implementation-facing record.

Feedback:

- The historical `default` label should be renamed to something such as
  `historical_default`; it must not remain the package default.
- The historical default was not one scientific source. It combined a common
  collection of log-linear CRFs with either historical GBD RRs (IERs) or a
  GBD-plus-GEMM selection. Here "GBD" refers to RRs, not epidemiological data.
- The legacy log-linear collection is broader than the current two-row
  `experimental_default` preset: it includes asthma, birth, work-absence, and
  mortality outcomes.
- `C40` was project-specific and `Krewski-South Africa` was country-specific.
  Preserve them as Sets used by their workflows, rather than presenting either
  as a universal scientific source preset.
- `NCD.LRI` is an aggregate of non-communicable diseases and lower respiratory
  infections. `TotResp`, `TotCV`, and `AllCause` are reference-dependent
  aggregate categories. Preserve or derive double-counting treatment from the
  relevant reference; do not assume one cause taxonomy applies everywhere.
- Do not prioritise only selected outcome families: migrate the complete legacy
  CRF collection together.
- Splitting `asthma_original`, `birth_original`, and `work_absence_original`
  is preferred. C40 and South Africa can each be their own Sets.
- The future formal package default needs a separate discussion with Jamie.
- Validation fixtures must not be user-facing.

Impact:

- Roadmap: expand the registry through one complete legacy-log-linear migration
  scope; keep promotion of the registry path and a formal default deferred.
- Step file: use the migration manifest as the required inventory, provenance,
  and parity-test contract for the log-linear migration.
- Preset/Set architecture: separate reusable outcome-family presets from
  historical project/country Sets; keep RR source selection distinct from EPI
  selection.
- Decision log: record historical-profile naming, complete migration scope, and
  fixture visibility as confirmed directions.
- Code/tests: before changing registry data, add coverage checks for every
  active legacy row, reference provenance, double-counting behaviour, and the
  exclusion of fixtures from user-facing discovery.

Follow-up questions:

- What is the approved name and composition of each historical-default variant
  (historical GBD/IER only versus historical GBD/IER plus GEMM)?
- Which components are covered by each `TotResp`, `TotCV`, and `AllCause`
  reference, and which concrete registry rows should be marked
  `double_counted = TRUE`?
- Does "migrate all together" cover only the active legacy log-linear CRF
  tables, or also other RR families generated by `generate_rrs()`?
- What formal default should the public API expose after Jamie's decision?

## 2026-08-24: Breaking-change boundary and fingerprint non-regression

Feedback:

- Do not preserve old workflow compatibility inside the breaking-change
  implementation merely to keep historical scripts running.
- Remove legacy selection parameters and their translation layers, including
  parameters such as `C_causes`, `GBD_causes`, and other old CRF/RR selectors.
- Users who need to reproduce an old release should install that old CREAHIA
  release. The new package should not carry a second compatibility API for
  historical calculations.
- Update repository workflow templates and examples to use the new Registry
  parameters rather than keeping them on the old interface.
- Keep fixture tests when useful, but prioritise numerical non-regression over
  parameter compatibility.
- When Registry defaults represent an old calculation, the new path must
  produce the same HIA/PAF results as the old path.
- Reuse `tests/testthat/test_results_immutability.R` as the model: generate
  fingerprints across representative versions and configurations, compare
  central estimates, and support explicitly marked authorised differences for
  known fixes.
- Adapt `generate_fingerprint()` for the new API. Newer package versions
  should call Registry parameters; older versions should continue to use their
  historical parameters. Compare resulting PAF values, not parameter names.

Impact:

- Roadmap: replace the legacy-workflow compatibility requirement with a clean
  breaking-change boundary; make workflow migration and numerical fingerprint
  testing prerequisites for legacy cleanup.
- Step file: update Step 4 to remove compatibility translation as active scope,
  and update Step 6 with a version-aware fingerprint/non-regression slice.
- Decision log: record that numerical equivalence is required for equivalent
  Registry configurations, while old parameter compatibility is not required.
- Code/tests: later adapt `generate_fingerprint()` and add Registry parameter
  sets for historical configurations; do not authorise changed snapshots until
  each difference has a documented cause.

Follow-up questions:

- Which exact Registry presets/Sets represent each historical fingerprint
  configuration, especially historical GBD/IER versus GBD-plus-GEMM variants?
- Which known numerical changes, if any, should be listed as authorised
  differences after the new fingerprint run?
