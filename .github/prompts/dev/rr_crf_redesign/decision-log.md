# RR/CRF Redesign Decision Log

## 2026-06-01: Keep roadmap and agent instructions separate

Decision:

- Put stable collaboration rules in `AGENTS.md`.
- Put redesign planning notes under `.github/prompts/dev/rr_crf_redesign/`.
- Ignore both from R package builds with `.Rbuildignore`.

Reason:

- `AGENTS.md` should stay short and reusable across conversations.
- Roadmap and step documents will change often as mentor feedback arrives.
- Development planning files should be versioned but should not ship as package
  contents.


## 2026-06-01: RR/CRF redesign branch and document location

Decision:

- All RR/CRF redesign development belongs on `feat/unify_rr_and_crf`.
- The source-of-truth worktree is `/Users/miao/Developer/creahia` unless a later
  instruction explicitly changes it.
- Development planning files live under
  `.github/prompts/dev/rr_crf_redesign/`.

Reason:

- Keeping this work on one branch avoids stale detached worktrees and misplaced
  edits.
- Keeping planning files under `.github/prompts` separates agent-facing prompts
  from package source and examples.

## Registry-first migration

Decision:

- Build and validate the CRF registry before changing the main compute flow.

Reason:

- The registry gives a testable contract for migrated CRFs.
- Validation catches bad metadata before runtime.
- It keeps early PRs reviewable.

## 2026-08-21: Keep validation fixtures out of user-facing CRF selection

Decision:

- Keep `registry_fixture` rows available for deterministic internal tests.
- Exclude them from registry search and available reference discovery.
- Reject them from packaged presets, preset descriptions, overrides, previews,
  and compute-ready CRF sets.
- Keep a tracked test inventory for the legacy CRF migration accounting rather
  than making tests depend on the private planning markdown file.

Reason:

- A validation fixture is not scientific evidence and must not appear as a
  selectable CRF alternative.
- The private migration manifest is intentionally not package data and may not
  exist in every checkout or test environment.
- A machine-readable test inventory can verify the 25 distinct legacy
  definitions and their 15/19/21 historical profile memberships, totalling 55
  source rows.

## Fixture provenance

Decision:

- Keep fake validation-only rows explicitly marked with `registry_fixture`.

Reason:

- Fixture rows are useful for validation tests but should not be confused with
  real migrated epidemiological evidence.

## First real tabular CRF

Decision:

- Use `PM25` + `IHD` + `Deaths` + `25+` from GEMM as the first real tabular
  migration.

Reason:

- It is important enough to exercise the new design.
- It is narrow enough for exact parity checks.

## Preserve standard PAF output

Decision:

- New CRF helpers should return:
  `pollutant`, `cause`, `outcome`, `region_id`, `low`, `central`, `high`.

Reason:

- This lets downstream impact calculation stay stable while the CRF/PAF layer is
  redesigned.

## Parity before replacement

Decision:

- Do not replace legacy PAF routing until helper-level parity tests are stable.

Reason:

- The refactor is a breaking-change effort, but numeric behavior still needs to
  be auditable during migration.

## 2026-06-01: Canonical form values should use constants

Decision:

- Use constants for CRF form values in validation, filtering, and dispatch.
- Keep `log_linear` as the canonical stored value for log-linear CRFs. Do not
  migrate it to `log-linear`.

Reason:

- The form value is stored in registry data and controls branching.
- Constants prevent hidden spelling mistakes.
- Keeping the existing `log_linear` value avoids unnecessary data churn.

## 2026-06-01: Rename concentration delta variables

Decision:

- Rename variables so concentration changes are not confused with absolute
  concentration values.
- Prefer names like `conc_change` and `conc_change_actual` for log-linear PAF
  calculations.

Reason:

- The formula uses concentration deltas.
- Ambiguous names such as `conc_ref`, `con_ref`, `source_conc`, or `source_cons`
  make it easy to misread the math.

## 2026-06-01: Breaking change means removing old compatibility layers

Decision:

- Future registry-driven code should remove compatibility translation for
  `C_causes`, `GBD_causes`, `RR_sources`, and `CRF_version`.

Reason:

- Keeping both old and new selection systems would make the breaking-change
  branch harder to reason about.
- The CRF registry should become the single source of truth for CRF selection.

## 2026-06-01: Remove historical NRT and `.region` branches

Decision:

- Remove pollutant-name parsing for `_NRT`.
- Remove `.region` branching and default to the GEMM data variant that includes
  China.

Reason:

- Pollutant names should not encode counterfactual behavior.
- Registry metadata should carry counterfactual values.
- The old `.region` branch adds complexity that is no longer needed.

## 2026-06-01: Return references with main results

Decision:

- The reporting-facing wrapper should return a list that includes both computed
  results and a references table.

Reason:

- Users need a copy-pasteable references table for reports.
- The registry gives enough metadata to derive references from selected CRFs.

## 2026-06-01: Defer age recoding bug fix

Decision:

- Do not fix the existing age interval `NA` behavior inside the CRF registry
  migration.

Reason:

- Fixing age recoding would widen the scope.
- The planned registry design can address age applicability with explicit rows,
  such as separate asthma rows for `1-18` and `18+`.

## 2026-06-15: Keep double-counting rules cause-level

Decision:

- `inst/extdata/crf/dc_groups.csv` stores double-counting cause relationships
  only.
- The file uses `dc_group`, `cause`, `role`, and `notes`.
- It must not include `pollutant` or `outcome`.
- It must not be used for EPI validation.
- Conflict detection and `add_double_counted()` apply these cause relationships
  within each `pollutant` + `outcome` group.

Reason:

- Double counting is caused by aggregate/component cause relationships, not by
  every pollutant/outcome combination.
- Expanding the rule file by pollutant and outcome would create repeated rows
  and make omissions easy.
- Grouping by `pollutant` and `outcome` at application time prevents rules from
  leaking across independent result groups.

## 2026-06-15: Preserve marking behavior for double-counted rows

Decision:

- Keep `add_double_counted()` as a marking step.
- Preserve explicit `double_counted` values from CRF metadata.
- Infer missing PM25/RR-style double-counting flags from `dc_groups.csv`.
- Do not block CRF selection by default because of double-counting conflicts.
- Keep selection-time double-counting checks available as an optional validation
  mode.

Reason:

- This preserves legacy behavior while replacing hard-coded cause lists with
  data-backed rules.
- Users may still need to inspect or intentionally select CRFs before deciding
  how to summarize results.
- Blocking selection by default would be a larger behavior change than this
  migration slice requires.

## 2026-06-15: Use presets instead of hard-coded bundles

Decision:

- Use the term preset for named CRF selections.
- Store packaged presets under `inst/extdata/crf/presets/`.
- Use `crfs_preset()` to load a named preset.
- Use `resolve_crf_selection()` to resolve a selection table into full registry
  rows.
- Do not keep the temporary `crfs_bundle()` API.

Reason:

- Presets are data, not core R logic.
- File-backed presets allow future team-maintained selections to be reviewed as
  CSV changes.
- `resolve_crf_selection()` better describes the function's role: it validates
  a compact selection table and returns matching registry rows.

## 2026-06-15: Keep compiler upsert helpers in shared utilities

Decision:

- `upsert_crf_registry_row()` and `upsert_crf_reference_row()` should live in
  `R/compile_crfs_utils.R`.
- Source-specific compilers such as `R/compile_crfs_gemm.R` should call the
  shared helpers rather than defining duplicate local copies.

Reason:

- Registry/reference upsert behavior is shared metadata-writing logic, not
  GEMM-specific CRF logic.
- Keeping one implementation avoids hidden function masking and makes future
  Fusion/GBD compiler work easier to review.

## 2026-06-15: Log-linear helper parity is complete before compute integration

Decision:

- Treat `apply_crf_log_linear()` as helper-level parity complete.
- Keep the direct parity test against legacy `compute_hia_paf_crfs()` before
  wiring registry-selected CRFs into the main compute path.
- Continue to defer legacy path cleanup until registry-to-compute integration is
  in place.

Reason:

- The new helper now proves both internal parameter routing and numerical
  consistency with one legacy NO2 log-linear CRF.
- The HIA top-level flow should remain stable while the registry-driven PAF path
  is connected in a small reviewable slice.

## 2026-06-15: Parity tests must guard against empty zero results

Decision:

- Registry-vs-legacy parity tests should not only assert equality.
- They should also assert that compared PAF values are not `NA` and not all zero.

Reason:

- If concentration grids and population data fail to align, both old and new
  paths can return zero.
- Equality between two all-zero outputs is a false positive, not evidence that
  the HIA calculation is correct.

## 2026-06-15: Track `epi_version` propagation as technical debt

Decision:

- Before deeper compute-path replacement, trace how `epi_version` reaches age
  weighting in both legacy and registry paths.
- Do not assume parity is fully understood until the legacy defaulting behavior
  is documented.

Reason:

- The registry tabular path passes `epi_version` explicitly for age weights.
- The legacy path appears to work without an explicit argument, likely through a
  lower-level default.
- Hidden defaults can make parity tests pass for the wrong reason.

Superseded by the 2026-06-29 clarification below for the legacy log-linear CRF
path. Tabular RR/CRF age-weighting still uses EPI data and should continue to
pass `epi_version` explicitly where possible.

## 2026-06-29: Legacy log-linear CRFs do not use EPI age weights

Decision:

- Treat the `epi_version` question as resolved for legacy log-linear CRFs.
- `compute_hia_paf_crfs()` computes population-weighted concentration changes
  and applies the log-linear PAF formula directly.
- It does not read EPI data.
- It does not do age-weighting.
- Keep `epi_version` explicit in the registry tabular path because
  `apply_crf_tabular()` uses EPI age weights.

Reason:

- Legacy log-linear CRFs and tabular RR/CRF calculations are different PAF
  mechanisms.
- The legacy log-linear CRF path only needs concentration, population,
  counterfactual, unit multiplier, RR values, and the reference concentration
  change.
- Tabular RR/CRF paths need age weights, so EPI versioning remains relevant
  there.
- Separating these two cases prevents accidental API decisions based on a false
  assumption that all CRF paths use EPI age weights.

## 2026-06-29: Registry compute path has compute-level parity coverage

Decision:

- Treat `compute_hia_paf_crfs_registry()` as implemented for the optional
  registry-driven CRF PAF path.
- Keep `compute_hia_paf(crf_compute = "legacy")` as the default until the public
  boundary decision is made.
- Use `compute_hia_paf(crf_compute = "registry")` for parity and migration
  checks.

Reason:

- Compute-level tests now cover one log-linear NO2 CRF against
  `compute_hia_paf_crfs()`.
- Compute-level tests now cover one tabular PM25/IHD CRF against
  `country_paf_perm()`.
- The parity tests include not-`NA` and not-all-zero guards, so equality is less
  likely to be a false positive from empty or degenerate fixture data.

## 2026-06-15: Initial Sets design direction (superseded by later implementation)

Decision:

- Keep file-backed presets as the current concrete selection layer.
- Explore a higher-level Sets layer for user-facing model selection and
  cause-specific overrides.
- Defer implementation of Sets until registry-to-compute integration and
  references output are stable.

Reason:

- Presets are precise and auditable but can expose long CRF IDs to users.
- Sets could let users choose a base model family and override specific causes
  without manually copying IDs.
- This is a UX/API design problem and should not block the current compute
  integration work.

This initial deferral was later superseded by the narrower opt-in
`crfs_set()` implementation recorded on 2026-07-16. The default public workflow
and broader preset coverage remain deferred.

## 2026-06-29: Revised Preset/Set direction from mentor feedback

Status:

- Partially confirmed on 2026-07-14.

Decision:

- Source/model presets such as `gbd2023` and `fusion` should not silently include
  unrelated original/log-linear CRFs.
- Original/log-linear CRFs should be moved into one or more explicit presets,
  such as `crfs_original`, or split by reference/outcome family.
- The Set creation function should support combining several presets.
- Use an API based on `presets=`, `add=`, `remove=`, and `replace=` instead of a
  single `overrides=` argument.
- `replace` should support the source-based form
  `pollutant + cause + outcome + reference_id`.
- `replace` should also support direct `crf_id` as a fallback.

Reason:

- Preset names should not imply a single scientific source if their contents mix
  multiple references and CRF types.
- Log-linear CRFs from asthma, work absence, and birth-related outcomes need to
  be visible as their own preset choices.
- Combining presets makes mixed-source workflows explicit instead of hidden
  inside a name like `gbd2023`.

Open question:

- Whether original/log-linear CRFs should be grouped into one preset or split by
  reference/outcome family.

## 2026-07-16: `crfs_set()` should trigger registry compute routing

Decision:

- When users pass a `crfs_set()` result to `compute_hia()`, route CRF PAF
  calculation through the registry path.
- Keep the default legacy path unchanged for ordinary CRF inputs.
- Treat this as public-boundary wiring, not as a full replacement of legacy CRF
  computation.

Reason:

- A `crfs_set()` object is already resolved to registry rows, so using the legacy
  CRF path would be inconsistent with the selected input shape.
- The change keeps the top-level flow stable:
  `compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()`.
- The change is narrow and reviewable because it only switches path when the
  caller explicitly provides registry-selected CRFs.

## 2026-07-16: Registry CRFs need explicit `double_counted` metadata

Decision:

- Add `double_counted` to registry rows before treating `crfs_set()` as stable
  for full end-to-end `compute_hia()` use.
- Validate that registry rows have non-missing `double_counted` values.
- Keep `dc_groups.csv` as the rule table for inferred aggregate/component cause
  relationships; do not move those relationship rules into `registry.csv`.

Reason:

- Legacy CRFs already carry explicit `double_counted` metadata.
- `compute_hia_impacts()` calls `add_double_counted()`, which preserves explicit
  CRF metadata before applying inferred double-counting rules.
- Since `registry.csv` is the single source of truth for CRFs, row-level CRF
  metadata needed downstream should live in registry rows rather than being
  patched in during compute.
- Adding one logical column is a schema change, but it is narrow: it preserves
  current legacy behavior and makes registry-selected CRFs compatible with the
  existing impact-marking step.

## 2026-07-16: Empty parsed `rr_sources` should not trigger RR PAF calculation

Decision:

- Treat a zero-row parsed `rr_sources` table as empty in `compute_hia_paf()`.
- Only call the RR-based PAF path when RR sources are actually present.
- Keep this as an internal routing fix rather than changing the public
  `rr_sources` API.

Reason:

- `compute_hia()` parses valid empty RR inputs into a tibble with columns
  `cause` and `source` but zero rows.
- `length(rr_sources)` counts columns for data frames, so a zero-row tibble was
  incorrectly treated as non-empty.
- Registry-only CRF workflows should be able to pass through
  `compute_hia_paf()` without accidentally invoking RR curve calculations.

## 2026-08-12: Keep an end-to-end registry smoke test beside migration tests

Decision:

- Add a focused `compute_hia()` smoke test for a `crfs_set()` registry-only
  workflow.
- Run the real CRF application, PAF coordinator, and impact multiplication.
- Supply a small EPI fixture and replace only external location lookup and
  diagnostic plotting dependencies so the test remains deterministic.
- Require a non-empty result with non-`NA`, non-zero impacts and preserved
  `double_counted` metadata.

Reason:

- Helper-level and compute-level parity tests do not prove that the public
  `compute_hia()` boundary reaches the registry PAF path and downstream impact
  calculation together.
- Keeping external services outside the test makes failures attributable to the
  registry-only HIA flow rather than network or plotting setup.

## 2026-08-17: Historical profiles are Sets, not the public default

Decision:

- Stop using `default` as the name or implication of a historical CRF profile.
  Use `historical_default` (or a reviewed successor) for the historical
  collection and keep `experimental_default` explicitly development/test-only.
- Treat C40 as a project-specific Set and Krewski-South Africa as a
  country-specific Set. Preserve them for their workflows and as examples of
  Set composition; do not present them as universal source/model presets.
- Keep the historical GBD/IER versus GBD-plus-GEMM choice separate from EPI
  versioning. In this context GBD refers to the RR/IER source, not GBD
  epidemiological data.

Reason:

- Historical names and wrapper arguments currently hide a mixture of common
  log-linear CRFs and different RR source choices.
- A source/model name must not silently claim ownership of CRFs from unrelated
  sources.

Open boundary:

- The exact identifiers and composition of historical-default variants remain
  subject to provenance recovery and Jamie's decision on the formal default.

## 2026-08-17: Migrate the complete active legacy log-linear collection

Decision:

- Migrate all rows in the three actively routed legacy CRF tables as one
  scientific coverage scope: 15 `historical_default`, 19 `c40`, and 21
  `krewski_south_africa` rows.
- A reviewable implementation may use several commits and tests, but no outcome
  family is a deferred scientific migration tier.
- Use `08-legacy-crf-migration-manifest.md` as the row-level contract.

Reason:

- The mentor rejected a mortality-first/morbidity-later classification.
- Partial coverage would make a new historical preset misleading.

## 2026-08-17: Make original outcome families explicit

Decision:

- Create separate original/log-linear preset candidates for asthma, birth, and
  work absence. A remaining mortality/original grouping is provisional until
  its reference provenance and final name are reviewed.
- Compose those presets into historical project/country Sets where applicable.

Reason:

- New models can replace individual outcome families, especially asthma,
  without obscuring the continuing source of other outcomes.

## 2026-08-17: Treat aggregate cause labels as reference-specific

Decision:

- `NCD.LRI`, `TotResp`, `TotCV`, and `AllCause` must carry reference-aware
  aggregate/component handling. Retain legacy `double_counted` evidence, then
  verify the target row flags and `dc_groups.csv` relationships from the cited
  source before broad registry migration.
- Do not infer that an `AllCause` or aggregate rule applies across pollutants
  merely because another reference uses a similarly named cause.

Reason:

- NCD.LRI combines NCDs and LRI; the other aggregate labels reflect reference
  taxonomies rather than a single package-wide cause hierarchy.

## 2026-08-17: Fixtures are internal-only

Decision:

- Validation fixtures must not appear in user-facing registry discovery,
  preset candidates, override options, or normal examples.

Reason:

- A fixture validates code mechanics; it is not epidemiological evidence a
  user should be able to select.

## 2026-08-24: Breaking change prioritises numerical equivalence over API compatibility

Decision:

- Treat the RR/CRF redesign as an intentional breaking change. Remove old
  parameter translation layers instead of preserving `C_causes`, `GBD_causes`,
  and related historical selectors in the new public interface.
- Update repository workflow templates and examples to use Registry presets or
  Sets before removing the old selectors.
- Define compatibility as numerical non-regression: an equivalent Registry
  configuration must reproduce the old implementation's central PAF/HIA
  estimates.
- Use a version-aware adaptation of `test_results_immutability.R` and
  `generate_fingerprint()` as the safety mechanism. Older package versions use
  historical arguments; the new version uses Registry inputs.
- Allow only narrowly scoped, documented authorised differences for known
  calculation fixes. An unexplained fingerprint change is a test failure.

Reason:

- Keeping old and new parameter systems together would retain confusing legacy
  concepts and conversion code in the redesigned package.
- Requiring numerical equivalence preserves the scientific result contract
  without forcing the new codebase to preserve an obsolete API.
- Fingerprints provide a repeatable check across multiple versions and expose
  unexpected changes below the public wrapper level.

Open boundary:

- The exact Registry preset/Set mapping for each historical fingerprint
  configuration must be established before changing the fingerprint
  generator, including historical GBD/IER-only and GBD-plus-GEMM variants.
