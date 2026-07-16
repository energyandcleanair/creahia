# CRF Discovery Helpers

## Purpose

The CRF discovery helpers make the registry-driven CRF workflow inspectable
before users run HIA with an explicit `crfs_set()` selection.

They help users answer six practical questions:

1. Which packaged CRF presets are available?
2. What CRFs does a preset actually select?
3. Which CRFs and references exist in the registry?
4. Given a preset-defined selection, what alternatives exist for matching HIA
   slots?
5. What would a preset-based CRF selection look like after previewed `add`,
   `remove`, and `replace` operations?
6. How can a previewed selection be resolved into registry CRF rows for compute?

These helpers are for inspection and selection planning. Except for
`crfs_set()`, they do not create a compute-ready selection. None of them run HIA,
compute PAFs, or modify package data.

## Core Concepts

### Registry

`inst/extdata/crf/registry.csv` is the single source of truth for CRFs. Each row
represents one CRF and includes fields such as:

- `crf_id`
- `pollutant`
- `cause`
- `outcome`
- `double_counted`
- `form`
- `reference_id`
- calculation metadata such as log-linear parameters or tabular data paths

The registry is the full list of CRFs that the package knows about. It does not
decide which CRF should be used by default.

### Preset

A preset is a human-readable selection manifest stored under
`inst/extdata/crf/presets/`. It chooses a subset of registry rows.

Conceptually, a preset says:

```text
For this pollutant / cause / outcome slot, use this crf_id.
```

The preset itself is intentionally compact. The full scientific metadata lives
in the registry.

### HIA Slot

For CRF selection, a health impact slot is identified by:

```text
pollutant + cause + outcome
```

Examples:

```text
PM25 / IHD / Deaths
NO2 / NCD.LRI / Deaths
NO2 / Asthma.1to18 / AsthmaIncidence
```

The same slot can have several registry CRFs from different references or
models. A preset selects one of them.

## Helper Reference

### `available_crf_presets()`

#### Purpose

List the packaged CRF presets available in the installed package data.

#### When To Use

Use this first when you do not know which preset names are available.

#### Example

```r
available_crf_presets()
```

Example output:

```r
[1] "experimental_default"
```

#### Interpretation

Each returned value can be passed to helpers such as `describe_crf_preset()`,
`crfs_preset()`, or `crf_override_options()`.

---

### `load_crf_preset(name)`

#### Purpose

Load a preset CSV as a compact selection table.

#### When To Use

Use this for low-level debugging of preset files. Most users should prefer
`describe_crf_preset()` because it joins the preset back to the registry and
shows scientific metadata.

#### Example

```r
load_crf_preset("experimental_default")
```

#### Interpretation

The result is a compact manifest with columns such as:

- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `notes`

This table is readable, but it is not the complete CRF metadata.

---

### `describe_crf_preset(name)`

#### Purpose

Show what a preset actually selects after resolving it against the CRF registry.

#### When To Use

Use this before adopting a preset in an analysis, a report, or a validation run.
It answers: "Which CRFs and references are included in this preset?"

#### Example

```r
describe_crf_preset("experimental_default")
```

#### Output Columns

- `preset`
- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `reference_id`
- `form`
- `notes`

#### Interpretation

If a row shows:

```text
pollutant: PM25
cause: IHD
outcome: Deaths
reference_id: burnett_2018_gemm
form: tabular
```

then the preset selects a tabular GEMM CRF for PM2.5-attributable IHD deaths.

---

### `search_crf_registry(...)`

#### Purpose

Search the full CRF registry with user-facing filters.

#### When To Use

Use this when you want to find CRFs by pollutant, cause, outcome, reference,
form, or free-text query.

#### Examples

Search one HIA slot:

```r
search_crf_registry(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Search by reference:

```r
search_crf_registry(reference_id = "burnett_2018_gemm")
```

Search by text:

```r
search_crf_registry(query = "gemm")
```

#### Output Columns

- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `reference_id`
- `form`
- `notes`

#### Interpretation

The registry can contain several CRFs for the same HIA slot. A search result
shows what exists; it does not say which one is selected by a preset.

---

### `available_crf_references()`

#### Purpose

List reference metadata available to CRF registry rows.

#### When To Use

Use this when you need to understand or choose a `reference_id`, especially
before planning a future `replace` entry.

#### Example

```r
available_crf_references()
```

#### Output Columns

- `reference_id`
- `author`
- `year`
- `title`
- `notes`

#### Interpretation

The `reference_id` is the compact key used in the registry. The author, year,
title, and notes explain what the key refers to.

---

### `crf_override_options(presets, pollutant = NULL, cause = NULL, outcome = NULL)`

#### Purpose

Display currently selected CRFs from one or more presets together with registry
alternatives for matching HIA slots.

This is an exploration helper. It does not perform replacement.

#### When To Use

Use this when you want to understand what a preset currently selects and what
could be used instead.

It supports partial filters, so you can start broad and then narrow down.

#### Examples

Inspect every selected slot and its alternatives:

```r
crf_override_options(
  presets = "experimental_default"
)
```

Inspect all PM2.5-selected slots:

```r
crf_override_options(
  presets = "experimental_default",
  pollutant = "PM25"
)
```

Inspect slots with IHD as the cause:

```r
crf_override_options(
  presets = "experimental_default",
  cause = "IHD"
)
```

Inspect death outcomes using a case-insensitive partial filter:

```r
crf_override_options(
  presets = "experimental_default",
  outcome = "death"
)
```

Inspect one exact HIA slot:

```r
crf_override_options(
  presets = "experimental_default",
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

#### Output Columns

- `pollutant`
- `cause`
- `outcome`
- `selected`
- `selected_by_preset`
- `crf_id`
- `reference_id`
- `form`
- `notes`

#### Interpretation

Rows with `selected = TRUE` are currently selected by the requested presets.
Rows with `selected = FALSE` are registry alternatives for the same matching
HIA slot.

For example, if the output contains:

```text
PM25 / IHD / Deaths / selected = TRUE / burnett_2018_gemm
PM25 / IHD / Deaths / selected = FALSE / registry_fixture
```

then the preset currently selects the Burnett GEMM CRF for PM2.5 IHD deaths,
and the registry contains another CRF for the same slot.

#### Important Boundary

`crf_override_options()` describes preset-defined selections only. It does not
represent a composed CRF set after `add`, `remove`, or `replace`.

Future replacement execution should still require precise slot information.
Exploration can be broad; modification should be exact.

---

### `preview_crf_set(presets, add = NULL, remove = NULL, replace = NULL)`

#### Purpose

Preview a composed CRF selection before running HIA.

This helper starts from one or more presets, then optionally previews explicit
`add`, `remove`, and `replace` operations. It returns a table that shows which
CRFs would be selected, added, removed, or replaced.

#### When To Use

Use this when you want to check a proposed CRF selection before turning it into
a `crfs_set()` object or passing selected CRFs into a compute workflow.

This is the main bridge between discovery helpers and the compute-ready Set API.

#### Basic Example

Preview the selected CRFs from one preset:

```r
preview_crf_set(
  presets = "experimental_default"
)
```

#### Add Example

Add a slot that is not currently selected by the preset:

```r
preview_crf_set(
  presets = "experimental_default",
  add = list(
    list(
      pollutant = "NO2",
      cause = "Asthma.1to18",
      outcome = "AsthmaIncidence",
      reference_id = "legacy_default_crfs"
    )
  )
)
```

You can also add by explicit `crf_id`:

```r
preview_crf_set(
  presets = "experimental_default",
  add = list(
    list(crf_id = "legacy_no2_asthma_1to18_incidence_v1")
  )
)
```

#### Remove Example

Remove a selected slot:

```r
preview_crf_set(
  presets = "experimental_default",
  remove = list(
    list(
      pollutant = "NO2",
      cause = "NCD.LRI",
      outcome = "Deaths"
    )
  )
)
```

Removed rows remain in the preview with `action = "removed"` so the user can see
what changed.

#### Replace Example

Replace one selected slot using `pollutant + cause + outcome + reference_id`:

```r
preview_crf_set(
  presets = "experimental_default",
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "registry_fixture"
    )
  )
)
```

You can also replace by explicit `crf_id`:

```r
preview_crf_set(
  presets = "experimental_default",
  replace = list(
    list(crf_id = "test_tabular_pm25_ihd_deaths_v1")
  )
)
```

#### Output Columns

- `pollutant`
- `cause`
- `outcome`
- `action`
- `crf_id`
- `reference_id`
- `form`
- `selected_by_preset`
- `notes`

#### Interpretation

The `action` column explains how each row entered the preview:

- `selected`: selected directly by the starting presets
- `added`: added by the `add` argument
- `removed`: selected by the starting presets, then marked for removal
- `replaced`: inserted by the `replace` argument

Rows added or replaced by user operations have `selected_by_preset = NA` because
they did not come directly from a packaged preset.

#### Important Boundaries

`add`, `remove`, and `replace` are precise operations. They should identify exact
HIA slots, not broad categories.

- `add` requires the slot to be absent from the current preview. If the slot is
  already present, use `replace`.
- `remove` requires `pollutant`, `cause`, and `outcome`.
- `replace` requires the slot to already be selected and not removed.
- `add` and `replace` support both source-based entries using
  `pollutant + cause + outcome + reference_id` and fallback entries using
  `crf_id`.

`preview_crf_set()` does not run HIA and does not create a `crfs_set()` object.
It only shows what the composed selection would look like.

---

### `crfs_set(presets, add = NULL, remove = NULL, replace = NULL)`

#### Purpose

Create a compute-ready CRF selection from one or more presets and optional
`add`, `remove`, and `replace` operations.

#### When To Use

Use this after inspecting a preset and previewing any changes with
`preview_crf_set()`. The result is resolved back to registry rows and can be
passed to `compute_hia()` or `compute_hia_paf()`.

#### Example

```r
crfs <- crfs_set(
  presets = "experimental_default"
)

compute_hia(
  conc_map = conc_map,
  species = "no2",
  regions = regions,
  epi = epi,
  rr_sources = tibble::tibble(
    cause = character(),
    source = character()
  ),
  crfs = crfs
)
```

#### Replace Example

```r
crfs <- crfs_set(
  presets = "experimental_default",
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "registry_fixture"
    )
  )
)
```

#### Interpretation

The returned object is a `creahia_crf_set`. It contains full registry rows,
including metadata needed downstream such as `reference_id`, `form`, and
`double_counted`.

When this object is passed to `compute_hia()` or `compute_hia_paf()`, the CRF PAF
calculation is routed through the registry path.

#### Important Boundaries

`crfs_set()` only composes registry-selected CRFs. It does not:

- change the package registry
- run HIA by itself
- guarantee that all legacy CRFs have already been migrated into the registry
- replace the default legacy path for ordinary CRF inputs

---

### `crfs_preset(name)`

#### Purpose

Load a preset and resolve it to full registry rows.

#### When To Use

Use this when code needs the actual selected registry rows, not only a
human-readable description.

#### Example

```r
crfs_preset("experimental_default")
```

#### Interpretation

This returns registry rows selected by the preset and validates that the
selection is well-formed.

---

### `resolve_crf_selection(selection)`

#### Purpose

Resolve an explicit selection table to registry rows.

#### When To Use

Use this for lower-level validation when a selection table already exists. Most
users should start from packaged presets instead.

#### Example

```r
selection <- tibble::tribble(
  ~pollutant, ~cause, ~outcome, ~crf_id,
  "PM25", "IHD", "Deaths", "gemm_pm25_ihd_25plus_deaths_v1"
)

resolve_crf_selection(selection)
```

#### Interpretation

The function checks that the selected `crf_id` exists and that each
`pollutant + cause + outcome` slot selects one CRF source.

---

### `crfs_override(crfs, pollutant, cause, outcome, crf_id)`

#### Purpose

Replace one selected CRF in an already resolved CRF selection.

#### When To Use

Use this as a low-level helper for direct replacement experiments. It is not the
planned final user-facing Set API.

#### Example

```r
crfs <- crfs_preset("experimental_default")

crfs_override(
  crfs,
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths",
  crf_id = "test_tabular_pm25_ihd_deaths_v1"
)
```

#### Interpretation

This changes one exact slot to a replacement `crf_id`. Unlike
`crf_override_options()`, this helper modifies the selection object that is
passed to it.

## Practical Workflows

### Workflow 1: Choose A Starting Preset

Start by listing available presets:

```r
available_crf_presets()
```

Then inspect a candidate:

```r
describe_crf_preset("experimental_default")
```

Use this workflow when you want to confirm which sources and CRF forms are
included before running or validating an HIA workflow.

### Workflow 2: Find CRFs For A Health Impact Slot

Search the registry:

```r
search_crf_registry(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Then inspect references:

```r
available_crf_references()
```

Use this workflow when you want to understand the scientific sources available
for a specific health impact slot.

### Workflow 3: Explore Replacement Options From A Preset

Start broad:

```r
crf_override_options(
  presets = "experimental_default",
  pollutant = "PM25"
)
```

Then narrow down:

```r
crf_override_options(
  presets = "experimental_default",
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Use this workflow when you know the current preset but need to understand which
CRFs could be considered as alternatives.

### Workflow 4: Preview A Custom CRF Selection

Use `crf_override_options()` to find the slot and possible references:

```r
crf_override_options(
  presets = "experimental_default",
  pollutant = "PM25"
)
```

Then preview the exact replacement:

```r
preview_crf_set(
  presets = "experimental_default",
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "registry_fixture"
    )
  )
)
```

This keeps the exploratory lookup separate from the exact modification.

### Workflow 5: Create A Compute-Ready CRF Set

Once the desired slot and reference are clear, keep the `crfs_set()` replace
entry precise:

```r
crfs <- crfs_set(
  presets = "experimental_default",
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

This preserves the distinction between exploratory lookup and actual
modification.

You can pass the result to the HIA compute path:

```r
compute_hia(
  conc_map = conc_map,
  species = "pm25",
  regions = regions,
  epi = epi,
  crfs = crfs
)
```

When `compute_hia()` receives a `crfs_set()` result, it uses the registry CRF PAF
path for CRF calculations.

## Current Limitations

- The current registry contains only a small number of migrated and fixture CRFs.
- Some alternatives may be validation fixtures rather than real epidemiological
  choices.
- `crf_override_options()` only reflects preset-defined selections. It does not
  account for previewed `add`, `remove`, or `replace` operations.
- `preview_crf_set()` is a preview table, not a `crfs_set()` object.
- `crfs_set()` is implemented for registry-selected CRFs, but registry coverage is
  still incomplete, so it should be treated as an opt-in migration path rather
  than the default CRF workflow.
