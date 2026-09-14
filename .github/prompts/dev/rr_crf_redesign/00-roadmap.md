# RR/CRF Redesign Roadmap

## Purpose

This work changes the concentration-response function layer so the package can
use one registry-driven CRF system for both:

- legacy log-linear CRFs
- tabular RR curves such as GEMM

The intended breaking change is to make CRF selection explicit, data-backed, and
testable instead of relying on several separate legacy paths.

## Breaking-change Boundary

This is an intentional breaking change. The new package should use the CRF
Registry as its selection contract rather than preserving a second interface
for historical parameters.

The breaking-change release should remove compatibility translation for old
selectors such as `C_causes`, `GBD_causes`, `RR_sources`, and `CRF_version`,
along with other obsolete CRF/RR argument names identified during cleanup.
Repository workflow templates and examples must be migrated to the Registry
interface before the old interface is removed.

Users who need to reproduce an old release should install that old CREAHIA
release. Reproduction does not require carrying old parameter names or
conversion logic in the new codebase.

Compatibility therefore means numerical non-regression, not parameter or
workflow compatibility: when a Registry preset/Set represents an old
calculation, its PAF/HIA results must match the old implementation. The
version-aware fingerprint plan is documented in Step 6.

## Current Direction

All RR/CRF redesign development belongs on the `feat/unify_rr_and_crf` branch.
Use `/Users/miao/Developer/creahia` as the source-of-truth worktree for this
branch unless a later instruction explicitly changes it.

Use `wrap-up-checklist.md` before returning to a new implementation slice or
after mentor feedback changes the plan.

Keep the existing high-level HIA flow stable while replacing the PAF/CRF layer in
small slices:

1. `compute_hia()` remains the top-level entry point.
2. `compute_hia_paf()` remains the PAF coordinator until the new CRF path is
   ready to replace legacy branches.
3. `compute_hia_impacts()` continues to consume the standard PAF output shape.
4. New CRF work should return rows shaped as:
   `pollutant`, `cause`, `outcome`, `region_id`, `low`, `central`, `high`.
5. The new breaking-change API should prefer CRF registry inputs and should not
   keep legacy argument translation layers. Any temporary exception must be
   explicitly documented and removed before release.

## Completed Slices

### 0. Collaboration and branch hygiene

Implemented:

- `AGENTS.md` records the stable collaboration rules for this package.
- RR/CRF planning files live under `.github/prompts/dev/rr_crf_redesign/`.
- Current development remains on `/Users/miao/Developer/creahia` branch
  `feat/unify_rr_and_crf`.

### 1. Registry and references

Implemented:

- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/references.csv`
- `load_crf_registry()`
- `load_crf_references()`
- `validate_crf_registry()`
- `validate_crf_references()`
- validation for required columns, duplicate IDs, valid forms, reference IDs,
  log-linear numeric fields, RR ordering, and tabular `data_path` existence

Tests:

- `tests/testthat/test_crf_registry.R`

### 2. First tabular GEMM migration

Implemented:

- `compile_gemm_ihd_25plus_deaths()`
- `validate_compiled_gemm_ihd()`
- `gemm_reference_row()`
- `upsert_crf_registry_row()`
- `upsert_crf_reference_row()`
- migrated data file:
  `inst/extdata/crf/tabular/gemm_pm25_ihd_25plus_deaths_v1.csv`
- registry row:
  `gemm_pm25_ihd_25plus_deaths_v1`

Tests:

- `tests/testthat/test_crf_gemm_migration.R`

### 3. Apply helpers for registry rows

Implemented:

- `apply_crf()`
- `apply_crf_tabular()`
- `load_crf_tabular()`
- `get_crf_age_weights()`
- `get_hazard_ratio_tabular()`
- `apply_crf_log_linear()`
- `calculate_log_linear_paf()`
- log-linear parameter-routing test
- log-linear parity test against legacy `compute_hia_paf_crfs()`

Tests:

- `tests/testthat/test_crf_apply.R`

### 4. Double-counting rules and preset selection

Implemented:

- `inst/extdata/crf/dc_groups.csv` now stores cause-level double-counting rules
  as `dc_group`, `cause`, `role`, and `notes`.
- `dc_groups.csv` is scoped to double-counting only. It does not include
  `pollutant` or `outcome`, and it should not be used for EPI validation.
- Double-counting rules are applied within each `pollutant` + `outcome` group,
  so cause relationships do not leak across pollutants or outcomes.
- `add_double_counted()` now infers component double-counting from
  `dc_groups.csv` while preserving explicit `double_counted` values from CRF
  metadata.
- `registry.csv` now carries `double_counted` as required CRF metadata.
- `validate_crf_registry()` requires non-missing `double_counted` values.
- `crfs_set()` output preserves `double_counted`, so registry-selected CRFs can
  pass through `compute_hia_impacts()` metadata handling.
- CRF presets are stored as CSV files under `inst/extdata/crf/presets/`.
- `crfs_preset()` loads packaged presets.
- `resolve_crf_selection()` resolves a `pollutant`/`cause`/`crf_id` selection
  table to registry rows.
- Selection-time double-counting checks are optional rather than enabled by
  default.

Tests:

- `tests/testthat/test_crf_double_counting.R`
- `tests/testthat/test_compute_hia_impacts.R`
- `tests/testthat/test_crf_select.R`

Follow-up from mentor review:

- Existing registry-vs-legacy parity tests should also assert that key PAF
  results are not all zero and not `NA`.
- This prevents false positives where both old and new calculations return zero
  because concentration grids and population data failed to align.

### 5. Constants in the new CRF path

Implemented:

- `CRF_FORM_LOG_LINEAR`
- `CRF_FORM_TABULAR`
- `CRF_FORM_VALUES`

These constants are used by the registry validation and new CRF apply/selection
helpers. The stored registry value remains `log_linear`.

### 6. Registry-driven CRF compute path

Implemented:

- `compute_hia_paf_crfs_registry()`
- optional `compute_hia_paf(crf_compute = "registry")` path
- compute-level parity test against legacy `compute_hia_paf_crfs()` for one
  log-linear NO2 CRF
- compute-level parity test against legacy `country_paf_perm()` for one tabular
  PM25/IHD CRF fixture
- not-`NA` and not-all-zero guard assertions on registry-vs-legacy parity tests
- `compute_hia_paf()` skips the RR-based path when `rr_sources` is an empty
  parsed table, supporting registry-only CRF workflows from `compute_hia()`

Tests:

- `tests/testthat/test_compute_hia_paf.R`

Notes:

- The registry path is wired as an optional compute path, not the default.
- The top-level HIA flow remains unchanged:
  `compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()`.
- Empty parsed `rr_sources` tables are treated as empty, so registry-only CRF
  workflows do not accidentally trigger RR-based PAF calculation.
- Legacy `compute_hia_paf_crfs()` for log-linear CRFs computes a
  population-weighted concentration delta and applies the closed-form PAF
  formula directly. It does not read EPI data or do EPI age-weighting.
- Tabular RR/CRF paths are different: they use age weights from long-format EPI
  data, so `epi_version` still matters for `apply_crf_tabular()` and
  `country_paf_perm()` parity tests.

### 7. CRF discovery and preview helpers

Implemented:

- `available_crf_presets()` lists packaged preset names.
- `describe_crf_preset()` returns a human-readable preset view after resolving
  preset rows back to `registry.csv`.
- `search_crf_registry()` searches registry CRFs by user-facing filters and
  returns a compact view.
- `available_crf_references()` lists reference IDs with author, year, title, and
  notes.
- `crf_override_options()` shows currently selected CRFs from one or more
  presets together with registry alternatives for matching HIA slots.
- `crf_override_options()` supports partial exploratory filters such as
  `pollutant = "PM25"`, `cause = "IHD"`, or `outcome = "death"`; actual future
  replacement should still use precise `pollutant + cause + outcome`
  identification.
- `preview_crf_set()` previews preset-based CRF selections and supports
  `add`, `remove`, and `replace` operations.
- `preview_crf_set(add = ...)` supports source-based additions with
  `pollutant + cause + outcome + reference_id` and fallback additions with
  `crf_id`.
- `preview_crf_set(remove = ...)` marks selected slots as `action = "removed"`
  while keeping the removed row visible in the preview.
- `preview_crf_set(replace = ...)` supports source-based replacement with
  `pollutant + cause + outcome + reference_id` and fallback replacement with
  `crf_id`.
- User-facing usage guidance was added in `doc/crf-registry-selection.md`.

Tests:

- `tests/testthat/test_crf_select.R`

Notes:

- These helpers are lookup and selection-inspection tools only.
- `preview_crf_set()` is a preview table, not the final `crfs_set()` object.
- `crf_override_options()` currently describes preset-defined selections. It
  does not represent a future composed set after add/remove/replace operations.
- Exact modification operations in `preview_crf_set()` should use complete HIA
  slots; broad partial filtering remains limited to discovery helpers such as
  `crf_override_options()`.

## Incomplete / Next Slices

### A. Naming and constants cleanup

Partially complete.

The CRF form constants exist and are used in the new registry/apply/selection
path. The new log-linear helper now uses explicit concentration-change naming
and has a direct parity test against the legacy CRF calculation. Remaining
cleanup is in legacy code:

- legacy `compute_hia_paf_crfs()` still uses `source_concs` for the actual
  concentration delta
- continue using constants instead of hard-coded form strings in new code

Canonical form spelling decision: keep the current stored value `log_linear`.
Do not migrate it to `log-linear`; this spelling is acceptable as long as code
uses constants instead of hard-coded strings.

### B. Clean up log-linear helper

Complete for helper-level parity.

`apply_crf_log_linear()` now has:

- a parameter-routing test that checks `effective_conc_change` is passed into
  the internal PAF calculator
- a direct numerical parity test against legacy `compute_hia_paf_crfs()` for one
  log-linear NO2 CRF

This means the helper is ready to support the next registry-to-compute
integration slice. The legacy `compute_hia_paf_crfs()` naming can be cleaned up
later when the legacy path is replaced.

### C. Move shared compiler utilities

Partially complete.

`R/compile_crfs_utils.R` now exists and contains:

- `upsert_crf_registry_row()`
- `upsert_crf_reference_row()`

The duplicate helper definitions have been removed from `R/compile_crfs_gemm.R`;
that file now calls the shared helpers.

### D. Expand GEMM compiler

The GEMM compiler should move from one IHD slice to a full GEMM dataset compiler.
Fusion and GBD compilers should come after the GEMM pattern is stable.

### E. Wire registry selection into the compute path

Complete for the optional PAF compute path and top-level `compute_hia()` boundary.

The selector contract is file-backed and registry-driven:

- `compute_hia_paf()` can consume selected registry CRFs through
  `compute_hia_paf_crfs_registry()` when called with
  `crf_compute = "registry"`.
- `compute_hia()` switches to the registry path when `crfs` is already a
  `crfs_set()` result.
- Registry-selected CRFs include `double_counted`, so they can pass through
  `compute_hia_impacts()` metadata handling.
- Empty parsed `rr_sources` tables are treated as empty, so registry-only CRF
  workflows do not accidentally trigger the RR-based PAF path.
- `tests/testthat/test_registry_smoke.R` runs the real
  `compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()` flow with a
  supplied EPI fixture and checks for non-zero, non-`NA` impacts.

The registry path is still opt-in for ordinary CRF inputs; it is not the global
default yet.

### F. Strengthen registry-vs-legacy parity tests

Complete for the current compute-level parity fixtures.

Registry-vs-legacy parity tests now include guard assertions that make sure
compared PAF values are meaningful:

- not `NA`
- not exactly zero, or not all zero for vector/data-frame outputs

This is required before larger compute-path replacement work, because equality
between two empty/zero outputs is not useful evidence.

### G. Confirm `epi_version` behavior

Complete for the legacy log-linear CRF path.

Confirmed behavior:

- legacy `compute_hia_paf_crfs()` computes PAF from CRF parameters and
  population-weighted concentration changes directly
- it does not read EPI data
- it does not do age-weighting
- therefore `epi_version` is not part of legacy log-linear CRF behavior

Important distinction:

- tabular RR/CRF calculations do use age weights from long-format EPI data
- the registry tabular path passes `epi_version` explicitly through
  `apply_crf_tabular()`
- legacy tabular parity through `country_paf_perm()` is a separate path from
  legacy log-linear `compute_hia_paf_crfs()`

### H. Design Sets above Presets

Presets are file-backed CRF selections. Mentor feedback revised the Sets design:

- source/model presets such as `gbd2023` and `fusion` should not silently include
  unrelated original/log-linear CRFs
- original/log-linear CRFs should live in one or more explicit presets, for
  example `crfs_original`, or split by reference/outcome family
- Sets should be able to combine several presets
- Sets should use `add=`, `remove=`, and `replace=` rather than one generic
  `overrides=` argument
- `replace` should support both source-based replacement by
  `pollutant + cause + outcome + reference_id` and direct `crf_id` replacement as
  a fallback
- user does not need to copy long CRF IDs into scripts
- discovery helpers now let users inspect available presets, registry rows,
  references, and preset-defined override options before the Set API is
  implemented
- `preview_crf_set()` now previews the first full composition surface
  (`presets`, `add`, `remove`, and `replace`) without creating a final Set object
  or changing compute flow

The selection and preview surfaces are now implemented for the revised API.
Further expansion is still gated on a stable registry selection/compute
contract, mentor confirmation of preset provenance, and broader registry
coverage.

### I. Replace legacy PAF routing

The new `apply_crf()` path is wired into `compute_hia_paf()` as an optional
registry-driven path. It is not yet the default.

Next replacement work should decide how to promote the registry path at the
public boundary, then remove old argument translation for `C_causes`,
`GBD_causes`, `RR_sources`, and `CRF_version` in a separate reviewable slice.

### J. Change main output shape

The main wrapper should return a list that includes both computed results and a
references table that users can copy into reports. The exact list structure
should be designed after registry selection is stable, because the references
table depends on selected CRFs.

### K. Expand registry coverage

The registry currently contains a small number of migrated/fixture rows. The
remaining legacy CRFs and additional tabular CRFs should be migrated in separate
reviewable slices.

### L. Complete the legacy log-linear migration as one scientific scope

Mentor feedback on 2026-08-17 changes the migration boundary. Do not choose a
subset of asthma, birth, absence, or mortality outcomes as a scientific first
batch: migrate the complete active legacy log-linear collection together.

Engineering work may still be split into small, reviewable commits, but the
registry must not publish a partial legacy replacement as if it were the
historical collection. The migration contract is documented in
`08-legacy-crf-migration-manifest.md` and currently covers all rows routed by
the active historical tables:

- `CRFs.csv` / `historical_default`: 15 rows
- `CRFs_C40.csv` / `c40`: 19 rows
- `CRFs_Krewski_SouthAfrica.csv` / `krewski_south_africa`: 21 rows

The implementation must preserve a distinction between:

- reusable, scientifically transparent presets such as
  `asthma_original`, `birth_original`, and `work_absence_original`;
- a provisional mortality/original preset whose final name and provenance still
  need review; and
- project/country Sets such as `c40` and `krewski_south_africa`, which compose
  the applicable presets and reference-specific overrides for a workflow.

The historical `default` name is deprecated for this purpose. Use a name such
as `historical_default` for the historical profile, while keeping the current
two-row `experimental_default` clearly labelled as development/test-only.

The first concrete readiness audit is
`10-default-profile-migration-readiness-audit.md`. It records the remaining
evidence required for the 15-row default profile without treating that profile
as a completed public migration.

Before adding migrated rows, recover missing bibliographic provenance, resolve
reference-specific aggregate-cause/double-counting behaviour, and add a test
that accounts for every manifest row. Do not change the public package default;
its meaning requires a separate discussion with Jamie.

### M. Keep validation fixtures internal

The current tabular fixture is useful for deterministic tests but must not be
user-facing. The next selector/registry data slice must choose and test one of
two implementations:

- move fixtures to a test-only registry/data fixture; or
- retain explicit internal metadata but exclude fixtures by default from every
  user-facing registry search, override option, preview, and preset candidate.

Until that choice is implemented, no fixture row may be added to a public
preset or used as an example of a scientific alternative.

Current implementation status:

- `registry_fixture` is retained in the registry for deterministic internal
  tests but is filtered from registry search and available references.
- Packaged presets, preset descriptions, override options, previews, direct
  overrides, and compute-ready sets reject internal fixture rows.
- User-facing documentation no longer presents the fixture as a replacement
  or alternative.
- A tracked test inventory under `tests/testthat/fixtures/` accounts for the
  25 distinct legacy definitions and their 15/19/21 profile memberships,
  totalling 55 historical source rows.

The full legacy CRF migration remains incomplete. This slice only protects
fixture visibility and verifies the historical row-accounting inventory; it
does not add missing scientific registry rows or resolve provenance.

### N. Numerical non-regression before legacy removal

Before removing the legacy compute path or accepting the new Registry path as
the replacement, adapt `tests/testthat/test_results_immutability.R` to compare
old-version calculations with equivalent Registry configurations. The test
must:

- use old parameters when loading an older package version;
- use Registry presets/Sets when calculating with the new package version;
- compare central PAF estimates for representative historical configurations;
- fail on unexpected missing rows or numerical breaks; and
- allow only documented, explicitly authorised differences for known fixes.

This is the primary safety mechanism for the breaking change. Existing
parameter-compatibility tests may remain as temporary development aids, but
they are not a requirement for the public API and must not block removal of old
arguments.

Current foundation:

- The nine historical RR/EPI/population configurations used by the existing
  immutability test are recorded in
  `tests/testthat/fixtures/historical_fingerprint_configurations.csv`.
- A focused contract test protects their legacy definitions and prevents an
  incomplete mapping from substituting `experimental_default` for a historical
  Registry target.
- All nine mappings remain `legacy_defined`, not Registry-ready, until their
  complete historical CRF rows and scientific metadata are available.

## Current Confirmation Points

These are the places where implementation and planning are close enough that the
next step should be confirmed before code cleanup:

- `compute_hia_paf()` still defaults to legacy RR/CRF routing. This remains an
  implementation transition point, not a commitment to preserve the legacy
  public parameter interface.
- `compute_hia_paf(crf_compute = "registry")` is available and covered by
  compute-level parity tests.
- `compute_hia()` switches to the registry path when users pass a `crfs_set()`
  result, without changing the default legacy behavior for ordinary CRF inputs.
- Registry rows now carry `double_counted` and `crfs_set()` output preserves it.
- Empty parsed `rr_sources` no longer trigger RR-based PAF calculation.
- The end-to-end registry-only smoke test now passes. The next implementation
  slice should adapt the fingerprint test to Registry-equivalent historical
  configurations, after registry coverage and preset provenance are expanded.

## Deferred Items

### EPI generation and EPI validation

Do not start EPI data generation from the current RR/CRF feedback. Also do not
use `dc_groups.csv` for EPI validation. `dc_groups.csv` is a double-counting
rule table, not an epidemiological coverage table.

### Age interval NA behavior

The existing age recoding behavior that can produce `NA` for age intervals such
as `5-9` and `10-14` should not be fixed inside the CRF registry migration. Keep
the behavior for now to avoid widening the scope. Future registry rows should
solve this by representing disease/age applicability explicitly, for example
separate asthma rows for `1-18` and `18+`.

## Working Rules

- Verify the active branch/worktree before editing.
- Keep each PR narrow enough to review.
- Add validation before depending on new data files.
- Add parity tests before replacing legacy behavior.
- Keep fake fixture rows clearly marked as fixtures.
- Treat this as a breaking-change branch: prefer clean new contracts over
  compatibility adapters once parity has been established.
