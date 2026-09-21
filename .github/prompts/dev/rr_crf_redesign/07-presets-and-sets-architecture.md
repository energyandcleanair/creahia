# Step 7: Presets and Sets Architecture

## Status

Mentor feedback received on 2026-06-29. The revised selection surface is now
implemented as an opt-in workflow; scientific preset grouping and promotion to
the default public workflow remain open.

## Goal

Improve the user experience for CRF selection without hiding scientific choices
or duplicating the CRF registry.

The design should support three needs:

- keep `registry.csv` as the single source of truth
- keep presets human-readable and reviewable
- avoid misleading preset names such as `gbd2023` or `fusion` silently including
  log-linear CRFs from unrelated references
- allow users to compose several presets and optionally add, remove, or replace
  selected CRFs

## Mentor Feedback Incorporated

Problems raised:

- Log-linear CRFs have historically been added to workflows even when the main
  wrapper argument says something like `gbd2023` or `fusion`.
- This creates a scientific transparency problem: users may think all outcomes
  came from GBD 2023 or Fusion RR curves, when some outcomes actually come from
  older log-linear CRFs.
- The old CRF table combines response functions from different references and
  different outcome families, such as asthma, work absences, and birth-related
  outcomes.

Revised direction:

- Keep source/model presets such as `gbd2023` and `fusion` focused on CRFs that
  actually belong to those sources.
- Move original/log-linear CRFs into one or more explicit presets.
- Make the Set creation function able to combine several presets.
- Use an API with `presets=`, `add=`, `remove=`, and `replace=` instead of a
  single `overrides=` argument.

## Registry Position

`registry.csv` is the single source of truth for CRFs.

It stores the full metadata and calculation information for each CRF, including:

- `crf_id`
- `pollutant`
- `cause`
- `outcome`
- `form`
- `reference_id`
- `age_low`
- `age_high`
- `data_path`

All presets and sets must eventually resolve back to concrete rows in
`registry.csv`.

`reference_id` is central to the proposed Set override design. The current
registry contains placeholder or inconsistent `reference_id` values in some
places, so those values must be cleaned before Set overrides are implemented.

## Preset Position

Short term, keep the current preset concept.

A preset is a human-readable selection manifest. It represents a reviewed subset
of the registry, for example a default GBD 2023 selection.

A preset is not a second registry. It should not duplicate all CRF metadata.

Preset CSV files should remain easy for humans to inspect. Proposed columns:

```text
pollutant
cause
outcome
crf_id
notes
```

Example:

```csv
pollutant,cause,outcome,crf_id,notes
PM25,IHD,Deaths,gemm_pm25_ihd_25plus_deaths_v1,Default PM2.5 IHD deaths CRF
NO2,Asthma.1to18,AsthmaIncidence,legacy_no2_asthma_1to18_incidence_v1,Default asthma incidence CRF
```

The `crf_id` links each preset row back to the registry. The other fields are
included for readability and review.

## Preset Naming And Scope

Preset names should describe the scientific source or role clearly.

Source/model presets should not silently include unrelated log-linear CRFs.

Examples:

- `gbd2023`: CRFs/RR-derived rows that genuinely belong to GBD 2023.
- `fusion`: CRFs/RR-derived rows that genuinely belong to Fusion.
- `crfs_original` or `original_crfs`: legacy/original log-linear CRFs grouped
  together.

If one original/log-linear preset becomes too broad, it can be split later by:

- reference, for example one preset per paper/source
- outcome family, for example `asthma_original`, `birth_original`,
  `work_absence_original`

The key design goal is that the preset name should not imply a single scientific
source when the contents mix multiple unrelated references.

### 2026-08-17 mentor direction: historical profiles and outcome families

The historical `default` table must not remain a package default. Refer to its
profile as `historical_default` (or a reviewed successor) and keep it distinct
from the current two-row `experimental_default` development/test preset.

The historical profile combined common log-linear CRFs with a choice of RR
source family: historical GBD/IER only, or historical GBD/IER plus GEMM. This
is an RR-source decision, not an EPI-version decision. A future selection call
must make that distinction visible rather than using a broad name that hides it.

The first reusable original/log-linear preset candidates are:

- `asthma_original`
- `birth_original`
- `work_absence_original`
- a provisional mortality/original preset, pending reference recovery and
  naming review

These presets are the scientific building blocks. `c40` and
`krewski_south_africa` should be historical project/country Sets composed from
the building blocks and any profile-specific reference choices. They should be
used directly by the relevant workflows, not advertised as a universal model
family.

The complete membership and outstanding provenance/double-counting work is
recorded in `08-legacy-crf-migration-manifest.md`.

## Set Position

A Set is a user-facing selection interface above presets.

A Set can combine several presets, then optionally apply add/remove/replace
operations.

The purpose is to let users express scientific choices without manually building
a full CRF selection table.

Example proposed interface for combining presets:

```r
crfs_set(
  presets = c("gbd2023", "crfs_original")
)
```

This means:

- use the GBD 2023 source/model preset
- also include the explicit original/log-linear CRF preset
- make the mixed-source nature visible in the user-facing call

Example proposed interface with source-based replacement:

```r
crfs_set(
  presets = c("gbd2023", "crfs_original"),
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "burnett_2018_gemm"
    ),
    list(
      pollutant = "NO2",
      cause = "Asthma.1to18",
      outcome = "AsthmaIncidence",
      reference_id = "some_asthma_reference"
    )
  )
)
```

In this design:

- `presets = c("gbd2023", "crfs_original")` combines several explicit presets.
- Each replacement identifies the slot to replace using
  `pollutant + cause + outcome`.
- `reference_id` specifies the preferred scientific source for the replacement
  CRF.
- The final result resolves to explicit registry rows so provenance remains
  clear.

Primary Set API:

```r
crfs_set(
  presets = c("gbd2023", "crfs_original"),
  add = list(...),
  remove = list(...),
  replace = list(...)
)
```

Meaning:

- `presets`: start from one or more named preset selections
- `add`: add CRFs or preset selections that are not already included
- `remove`: remove selected pollutant/cause/outcome rows
- `replace`: replace selected pollutant/cause/outcome rows with CRFs from a
  requested `reference_id` or explicit `crf_id`

## Proposed Resolver Meaning

Each replacement means:

```text
In the current combined preset selection, replace the CRF for this
pollutant/cause/outcome with the CRF from registry.csv that has the same
pollutant/cause/outcome and the requested reference_id.
```

For example:

```r
list(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths",
  reference_id = "burnett_2018_gemm"
)
```

means:

```text
For PM2.5 / IHD / Deaths, use the CRF whose source is burnett_2018_gemm.
```

The primary replacement form is source-based:

```r
list(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths",
  reference_id = "burnett_2018_gemm"
)
```

An explicit `crf_id` replacement should also be supported as a fallback:

```r
list(
  crf_id = "gemm_pm25_ihd_25plus_deaths_v1"
)
```

If `crf_id` is used, the resolver should look up the replacement row directly in
`registry.csv`. The implementation may still require or infer the replacement
slot from the matched registry row so the final selection remains unique.

## Proposed Preset Composition Rules

Combining several presets should be explicit and validated.

Proposed first rule:

- if two presets contain the same `pollutant + cause + outcome` slot, this is a
  conflict unless the user resolves it with `replace=` or `remove=`
- if two presets contain distinct slots, they can be combined
- the final selection must still resolve to unique registry rows

This prevents `crfs_set(presets = c("gbd2023", "crfs_original"))` from silently
selecting two CRFs for the same slot.

## User Discovery Helpers

The Set interface depends on users being able to inspect presets and search the
registry without manually opening CSV files.

Proposed helper functions:

### `available_crf_presets()`

List available packaged presets.

Example use:

```r
available_crf_presets()
```

### `describe_crf_preset(name)`

Return a human-readable view of a preset after joining it back to the registry.

Example use:

```r
describe_crf_preset("gbd2023")
```

Expected output should show fields such as:

- `preset`
- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `reference_id`
- `form`
- `notes`

### `search_crf_registry(...)`

Search available CRFs by user-facing filters.

Example use:

```r
search_crf_registry(pollutant = "PM25", cause = "IHD")
search_crf_registry(reference_id = "burnett_2018_gemm")
search_crf_registry(query = "asthma")
```

The default output should be a compact, human-readable registry view, not every
technical column.

### `crf_override_options(presets, pollutant = NULL, cause = NULL, outcome = NULL)`

Show currently selected CRFs from one or more presets together with registry
alternatives for matching HIA slots.

This is an exploratory helper, not a replacement executor. It supports partial
filters so users can browse broadly before narrowing down to one exact slot.

Example use:

```r
crf_override_options(
  presets = c("gbd2023", "crfs_original")
)

crf_override_options(
  presets = c("gbd2023", "crfs_original"),
  pollutant = "PM25"
)

crf_override_options(
  presets = c("gbd2023", "crfs_original"),
  outcome = "death"
)

crf_override_options(
  presets = c("gbd2023", "crfs_original"),
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Expected output should show:

- matching `pollutant + cause + outcome` slots
- which CRFs are currently selected by the requested presets
- alternative registry rows for the same matching slots
- the `reference_id` values that may inform a Set replacement

Important boundary:

- `crf_override_options()` describes preset-defined selections only.
- It does not preview a composed `crfs_set()` result after `add`, `remove`, or
  `replace`; use `preview_crf_set()` for that.
- Exploratory filtering can be partial, but actual replacement should still use
  precise slot identification.

### `preview_crf_set(presets, add, remove, replace)`

Preview the result of composing presets and applying add/remove/replace changes
before running HIA.

Example use:

```r
preview_crf_set(
  presets = c("gbd2023", "crfs_original"),
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "burnett_2018_gemm"
    )
  )
)
```

Expected output should show what changed, for example:

- `pollutant`
- `cause`
- `outcome`
- action such as `selected`, `added`, `removed`, or `replaced`
- `crf_id`
- `reference_id`
- `form`
- `selected_by_preset`
- `notes`

Current implemented preview behavior:

- `presets` loads one or more packaged presets and resolves them to registry
  rows.
- `add` inserts a slot that is not currently selected. It supports
  `pollutant + cause + outcome + reference_id` and fallback `crf_id` forms.
- `remove` marks an existing selected slot as `action = "removed"` while keeping
  the row visible in the preview.
- `replace` swaps an existing selected slot with a registry CRF. It supports
  `pollutant + cause + outcome + reference_id` and fallback `crf_id` forms.

Important boundary:

- `preview_crf_set()` is still a preview table, not the final `crfs_set()`
  object.
- It does not run HIA and does not change the compute flow.
- Broad partial filtering belongs in discovery helpers such as
  `crf_override_options()`; modification operations should use exact slots.

### `crfs_set(presets, add, remove, replace)`

Create a compute-ready CRF selection from the same composition inputs as
`preview_crf_set()`.

Current implemented behavior:

- `crfs_set()` calls `preview_crf_set()` for composition and validation.
- Rows marked `action = "removed"` are dropped from the final selection.
- The remaining rows are resolved back to full registry rows.
- The returned object has class `creahia_crf_set`.
- Passing a `creahia_crf_set` to `compute_hia()` or `compute_hia_paf()` routes CRF
  PAF calculation through the registry path.
- Registry rows include `double_counted`, so `compute_hia_impacts()` can preserve
  explicit CRF metadata during double-counting marking.

Important boundary:

- `crfs_set()` is implemented for registry-selected CRFs, but registry coverage
  remains limited.
- It is an opt-in migration path, not the default CRF workflow.
- It does not solve the open question of how to group original/log-linear CRFs
  into future packaged presets.

### `available_crf_references()`

List available references from the registry/reference table.

Example use:

```r
available_crf_references()
```

Expected output should help users choose `reference_id` values for overrides,
including fields such as:

- `reference_id`
- `author`
- `year`
- `title`
- `notes`

## Why Not Use `selection_group`

Earlier designs considered a synthetic `selection_group`, such as
`pm25_ihd_deaths`.

That design is not preferred for now because users would still need to look up a
new package-specific key. Using the existing fields `pollutant`, `cause`, and
`outcome` is more explicit and avoids inventing another naming system.

## Why Not Use Only `crf_id`

`crf_id` should remain the precise internal key, but it is not the preferred
user-facing override input.

Users usually think in terms of:

```text
pollutant + health outcome + scientific source
```

not in terms of long internal CRF IDs.

Therefore, the proposed primary Set override interface uses:

```text
pollutant + cause + outcome + reference_id
```

An advanced fallback may still allow direct `crf_id` overrides later, but that is
not the main user-facing design. This fallback is now accepted in the planned API
because it is useful when `reference_id` is ambiguous or not yet cleaned.

## Explicit Non-goals For Now

The following remain out of scope until mentor review:

- do not add `selection_group`
- do not create many small override preset CSV files
- do not implement broad registry query rules
- do not replace the current preset design with a second registry
- do not let source/model presets silently include unrelated original/log-linear
  CRFs
- do not expand Sets into a broader public-default workflow until `reference_id`
  values, preset coverage, and original/log-linear preset grouping are cleaned up
- do not add a separate `preset_crf_slots()` helper for now; `describe_crf_preset()`
  and `crf_override_options()` should cover the practical use cases
- do not add a standalone `validate_crf_overrides()` helper unless a real need
  appears; `crfs_set()` and `preview_crf_set()` should perform the same
  validation internally and return clear errors

## Mentor Review Questions

- Is it acceptable for Presets to remain human-readable CSV selection manifests
  with `pollutant`, `cause`, `outcome`, `crf_id`, and `notes`?
- Is `pollutant + cause + outcome + reference_id` a good user-facing override
  interface for Sets?
- Should original/log-linear CRFs be grouped in one preset such as
  `crfs_original`, or split by reference/outcome family such as
  `asthma_original`, `birth_original`, and `work_absence_original`?
- Are the proposed discovery helpers enough for users to inspect presets,
  references, and override options?
- Are current or planned `reference_id` values stable enough to support this API
  after cleanup?

## Confirmed Direction As Of 2026-07-14

- Use `crfs_set(presets = ..., add = ..., remove = ..., replace = ...)` as the
  planned Set API shape.
- Keep source-based replacement with
  `pollutant + cause + outcome + reference_id` as the main user-facing form.
- Also support direct `crf_id` replacement as a fallback.

Remaining open question:

- Whether original/log-linear CRFs should be grouped into one preset such as
  `crfs_original`, or split into presets such as `asthma_original`,
  `birth_original`, and `work_absence_original`.

## Implementation Status and Timing

Core implementation status:

1. Discovery helpers are implemented.
2. `preview_crf_set()` is implemented for `presets`, `add`, `remove`, and
   `replace`.
3. `crfs_set()` is implemented as an opt-in compute-ready selection object.
4. Registry-selected CRFs are wired into the optional registry compute path.

Do not promote this to the default workflow until:

1. `reference_id` values in `registry.csv` are cleaned.
2. Source/model presets are separated from original/log-linear CRF presets.
3. Remaining legacy CRFs and required tabular CRFs are migrated into the registry.
4. A references-table output contract is designed and tested.

The end-to-end `compute_hia()` smoke-test gate is now complete:
`tests/testthat/test_registry_smoke.R` covers a registry-only `crfs_set()`
workflow. The first four items remain before considering promotion to the
default workflow.

The first end-to-end smoke test is now implemented in
`tests/testthat/test_registry_smoke.R`. It runs the real
`compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()` flow with a
small supplied EPI fixture and mocks only external location/plot services, not
CRF or PAF calculations. The test passes and confirms non-zero, non-`NA`
registry-only impacts.
