# CRF Registry Discovery and Selection

## Purpose

The CRF Registry helpers make concentration-response function (CRF) selection
explicit and inspectable before running a health impact assessment (HIA).

Use them to:

1. list the packaged CRF presets;
2. inspect the Registry rows selected by a preset;
3. search available CRFs and their references;
4. compare alternatives for the same HIA slot;
5. preview `add`, `remove`, and `replace` operations; and
6. create a compute-ready `crfs_set()` object.

The discovery and preview helpers do not calculate population attributable
fractions (PAFs) or health impacts. `crfs_set()` creates a selection that the
existing compute flow can use:

```text
compute_hia() -> compute_hia_paf() -> compute_hia_impacts()
```

## Current Status and Scope

The public Registry now contains real, opt-in CRFs for GEMM, Fusion, GBD 2019,
and two migrated log-linear definitions. The examples in this document use
only those user-facing Registry rows. Internal validation fixtures are excluded
from public search, preset, preview, and Set workflows. Internal development
presets are omitted from public discovery and are never defaults. They can be
loaded by explicit name, but are for development only, are not reviewed, and
are not recommended methods.

The two public `legacy_` rows were migrated from the previous `CRFs.csv` table.
They remain searchable and selectable, but are not methodologically reviewed or
recommended defaults. `legacy_no2_ncdlri_deaths_v1` cites
`legacy_default_crfs`, a temporary provenance record rather than a scientific
source. `legacy_no2_asthma_1to18_incidence_v1` cites
`khreis_2017_childhood_asthma`; a recovered citation is not a methodology
endorsement.

The strongest replacement example currently available is the
`PM25 / IHD / Deaths` slot:

- `gemm_pm25_ihd_adult_deaths_v1` is part of the GEMM adult component preset;
- `fusion_pm25_ihd_adult_deaths_v1` is part of the Fusion adult component
  preset; and
- both have end-to-end parity tests against their corresponding legacy paths.

This makes GEMM-to-Fusion replacement suitable for demonstrating the selection
API. It does not mean that one source is scientifically preferred over the
other. Source choice remains an analysis decision.

Registry coverage is still narrower than all historical package behavior.
In particular, the public presets are opt-in and do not by themselves establish
a new package default or complete the migration of every legacy outcome and
workflow.

## Core Concepts

### Registry

`inst/extdata/crf/registry.csv` is the source of truth for public CRF
definitions. Each row describes one CRF and includes:

- its identity: `crf_id`;
- its HIA slot: `pollutant`, `cause`, and `outcome`;
- its form: `log_linear` or `tabular`;
- its source: `reference_id`;
- its age and regional applicability;
- its double-counting metadata; and
- the parameters or data path required for calculation.

The Registry says which CRFs are available. It does not choose a default CRF
for an analysis.

### Preset

A preset is a compact selection manifest under
`inst/extdata/crf/presets/`. Each row maps one HIA slot to one `crf_id`:

```text
pollutant + cause + outcome -> crf_id
```

The Registry stores the full scientific and calculation metadata; the preset
stores the intended selection.

The adult mortality presets distinguish two representations:

- an aggregate preset selects `NCD.LRI`; and
- a component preset selects its individual causes.

Do not combine an aggregate preset with the corresponding component preset in
one calculation. That would count overlapping mortality burden twice.

### HIA Slot

The selection API identifies a health impact slot by:

```text
pollutant + cause + outcome
```

Examples include:

```text
PM25 / IHD / Deaths
NO2 / NCD.LRI / Deaths
NO2 / Asthma.1to18 / AsthmaIncidence
```

Several Registry rows may occupy the same slot. For example, GEMM, Fusion, and
GBD 2019 all provide a public CRF for `PM25 / IHD / Deaths`. A preset selects
one of those alternatives.

## Recommended Workflow

The normal sequence is:

```r
# 1. Discover presets.
available_crf_presets()

# 2. Inspect the starting preset.
describe_crf_preset("gemm_adult_deaths_components")

# 3. Compare alternatives for one slot.
crf_override_options(
  presets = "gemm_adult_deaths_components",
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)

# 4. Preview an exact replacement.
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)

# 5. Create the compute-ready selection.
crfs <- crfs_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)

# Confirm that the final resolved rows have one CRF per HIA slot and no
# double-counting conflict.
validate_crf_selection(crfs, check_double_counting = TRUE)
```

This sequence keeps discovery, review, and execution separate. It also makes a
mixed-source Set visible before it is used in HIA.

## Practical Workflows

### Workflow 1: Choose A Starting Preset

Choose a model-family preset whose aggregate/component representation matches
the analysis. For example, start with the GEMM adult component curves:

```r
available_crf_presets()
describe_crf_preset("gemm_adult_deaths_components")
```

This selects the individual adult mortality causes `LRI`, `COPD`, `IHD`, `LC`,
and `Stroke`. If the analysis instead requires the aggregate `NCD.LRI` curve,
choose `gemm_adult_deaths_aggregate`; do not combine the two.

### Workflow 2: Find CRFs For A Health Impact Slot

Search the Registry when you know the pollutant, cause, and outcome but have not
yet chosen a source:

```r
search_crf_registry(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

The result lets you compare the available GEMM, Fusion, and GBD 2019 rows for
the same slot. Inspect their source metadata before choosing one:

```r
available_crf_references()
```

### Workflow 3: Explore Replacement Options From A Preset

Start from the GEMM component preset and inspect public alternatives for its IHD
slot:

```r
crf_override_options(
  presets = "gemm_adult_deaths_components",
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Rows with `selected = TRUE` are the current GEMM selection. Rows with
`selected = FALSE` are alternatives, including the public Fusion and GBD 2019
IHD curves. Use `crf_id` to distinguish age structures when a source has more
than one row for the same slot.

### Workflow 4: Preview A Custom CRF Selection

Preview replacing the selected GEMM IHD curve with the public Fusion adult-age
curve:

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)
```

The preview should show the Fusion row with `action = "replaced"`. This is a
mixed-source selection, so record why the Fusion source is preferred for IHD.
The same replacement can be written with exact slot metadata:

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "burnett_2022_fusion"
    )
  )
)
```

### Workflow 5: Create A Compute-Ready CRF Set

After reviewing the preview, pass the same exact operation to `crfs_set()`:

```r
crfs <- crfs_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)
```

You can then pass `crfs` to the existing HIA compute flow together with the
analysis inputs:

```r
hia <- compute_hia(
  conc_map = conc_map,
  species = "pm25",
  regions = regions,
  epi = epi,
  crfs = crfs
)
```

The Set is the complete method selection, so omitting `rr_sources` does not add
the default legacy RR curves. Add the required RR-derived CRF to the Set instead
of selecting an overlapping slot through both interfaces; explicit overlaps
are rejected before PAF calculation.

The Set is compute-ready, but creating it does not run HIA or change the
Registry. Ordinary legacy CRF inputs and package defaults remain unchanged.

### Workflow 6: Inspect An Unchanged Legacy Configuration

HIA users can keep their existing wrapper arguments and inspect how those
selectors resolve before running the calculation. For example, define the same
C40 cause vector used by the wrapper call:

```r
causes_to_include <- c(
  "Stroke_YLDs",
  "Diabetes_YLDs",
  "COPD_YLDs",
  "NCD.LRI_Deaths",
  "IHD_Deaths",
  "Stroke_Deaths",
  "COPD_Deaths",
  "LC_Deaths",
  "LRI_Deaths",
  "LRI.child_Deaths",
  "Diabetes_Deaths"
)

inspection <- inspect_hia_configuration(
  crfs_version = "C40",
  epi_version = "C40",
  calc_causes = causes_to_include,
  gbd_causes = "default",
  pop_year = 2022
)

inspection
```

The printed summary reports whether the call can use Registry routing, its
canonical input contract, component counts, and reference count. Inspect the
underlying tables when more detail is needed:

```r
inspection$components %>%
  dplyr::select(
    component_type,
    component_id,
    source,
    cause,
    outcome,
    age_contract,
    lifecycle,
    routing_status,
    scientific_review_status
  )

inspection$registry_set %>%
  dplyr::select(
    crf_id,
    pollutant,
    cause,
    outcome,
    form,
    reference_id
  )

inspection$references %>%
  dplyr::select(reference_id, author, year, title, doi, url)
```

This inspection does not calculate PAFs or impacts, alter routing options, or
trigger diagnostic observers. It uses the same compatibility resolver as the
production route, so it explains the selection without maintaining a second
legacy-to-Registry mapping.

## Helper Reference

### `inspect_hia_configuration(...)`

Explains how legacy HIA selectors resolve at the compatibility boundary without
running HIA.

The main output fields are:

- `route`: `registry`, `legacy`, or `unchanged`;
- `reason`: why that route was planned;
- `input_contract`: the canonical component-selection identity;
- `canonical_specification`: the structured CRF, RR, EPI, and population-year
  specification used by routing;
- `components`: one table containing all resolved CRF, RR, and EPI components;
- `registry_set`: the compute-ready Registry Set when all components are ready;
- `references`: bibliographic metadata used by the resolved Registry Set;
- `blocking_components`: components preventing Registry routing; and
- `population_year`: supplied and resolved population-year intent.

For a Registry-ready call, `registry_set` and `references` contain the exact
inputs that the compatibility route would pass to the shared engine. Passing
that `registry_set` back as `crfs` reproduces the results of the original
legacy-shaped call, including its missing-value handling, regional error
isolation, and `double_counted` marking. For an
unready call, `route` is `legacy`, `registry_set` is `NULL`, and
`blocking_components` explains what is missing. Definitions marked
`historical_reproduction` or `not_reviewed` support compatibility only and must
not be interpreted as approved public methodology. EPI is validated at runtime
and therefore does not use the CRF/RR Registry lifecycle label.

### `available_crf_presets()`

Lists the packaged preset names.

```r
available_crf_presets()
```

At the time of writing, the main migrated presets are:

```text
fusion_adult_deaths_aggregate
fusion_adult_deaths_components
gbd2019_deaths
gemm_adult_deaths_aggregate
gemm_adult_deaths_components
```

Use the function rather than hard-coding this list because packaged presets may
change as the migration progresses.

The adult aggregate and component presets are alternatives, not additive
building blocks:

- `gemm_adult_deaths_aggregate` selects GEMM `NCD.LRI` deaths;
- `gemm_adult_deaths_components` selects GEMM `LRI`, `COPD`, `IHD`, `LC`, and
  `Stroke` deaths;
- `fusion_adult_deaths_aggregate` selects Fusion `NCD.LRI` deaths; and
- `fusion_adult_deaths_components` selects Fusion `LRI`, `COPD`, `Diabetes`,
  `IHD`, `LC`, and `Stroke` deaths.

### `load_crf_preset(name)`

Loads the compact CSV manifest for one preset.

`name` is required; use `available_crf_presets()` to find public preset names.

```r
load_crf_preset("gemm_adult_deaths_components")
```

The result contains:

- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `notes`

Use this mainly for debugging preset files. For analysis review, prefer
`describe_crf_preset()`, which joins the manifest to Registry metadata.

### `describe_crf_preset(name)`

Shows the full identity of the CRFs selected by a preset.

```r
describe_crf_preset("fusion_adult_deaths_components")
```

The output contains:

- `preset`
- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `reference_id`
- `form`
- `notes`

Use this before adopting a preset in an analysis. For example, a row with
`reference_id = "burnett_2022_fusion"` and `form = "tabular"` identifies a
tabular Fusion CRF; the preset name alone should not be treated as sufficient
scientific documentation.

### `search_crf_registry(...)`

Searches user-facing Registry rows by exact metadata filters or a
case-insensitive text query.

Search one HIA slot:

```r
search_crf_registry(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Search one source:

```r
search_crf_registry(reference_id = "burnett_2022_fusion")
```

Search across IDs, slot metadata, references, forms, and notes:

```r
search_crf_registry(query = "gemm")
```

The output contains:

- `pollutant`
- `cause`
- `outcome`
- `crf_id`
- `reference_id`
- `form`
- `notes`

A search result shows what is available. It does not imply that a row is the
package default or the scientifically preferred choice.

### `available_crf_references()`

Lists the public reference metadata used by Registry rows.

```r
available_crf_references()
```

The output contains:

- `reference_id`
- `author`
- `year`
- `title`
- `notes`

Use this to interpret source keys such as `burnett_2018_gemm`,
`burnett_2022_fusion`, and `ihme_2021_gbd2019_pm_risk_curves`. Internal fixture
references are not returned.

### `crf_override_options(...)`

Shows the CRFs currently selected by one or more presets together with public
Registry alternatives for the same HIA slots. It does not change a selection.

Inspect all alternatives for the GEMM adult component slots:

```r
crf_override_options(
  presets = "gemm_adult_deaths_components"
)
```

Inspect one exact slot:

```r
crf_override_options(
  presets = "gemm_adult_deaths_components",
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

Partial filters are useful during exploration:

```r
crf_override_options(
  presets = "gemm_adult_deaths_components",
  outcome = "death"
)
```

The output contains:

- `pollutant`
- `cause`
- `outcome`
- `selected`
- `selected_by_preset`
- `crf_id`
- `reference_id`
- `form`
- `notes`

For `PM25 / IHD / Deaths`, the result includes the selected GEMM adult curve
and public alternatives from Fusion and GBD 2019. It may also include the older
GEMM `25+` aggregate curve. Because multiple GEMM rows share the same slot and
reference, use `crf_id` when the intended age structure must be unambiguous.

`crf_override_options()` always starts from packaged presets. It does not
describe a composition that has already been changed by `add`, `remove`, or
`replace`; use `preview_crf_set()` for that.

### `preview_crf_set(...)`

Previews a composed selection without creating a compute-ready Set or running
HIA.

#### Start From A Preset

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components"
)
```

#### Add A Missing Slot

Add a real, migrated NO2 childhood-asthma CRF by `crf_id`:

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  add = list(
    list(crf_id = "legacy_no2_asthma_1to18_incidence_v1")
  )
)
```

The equivalent source-based form is:

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  add = list(
    list(
      pollutant = "NO2",
      cause = "Asthma.1to18",
      outcome = "AsthmaIncidence",
      reference_id = "khreis_2017_childhood_asthma"
    )
  )
)
```

#### Remove A Selected Slot

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  remove = list(
    list(
      pollutant = "PM25",
      cause = "LC",
      outcome = "Deaths"
    )
  )
)
```

Removed rows remain visible with `action = "removed"`.

#### Replace A Selected Slot

Replace GEMM IHD with the corresponding public Fusion adult-age curve:

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)
```

The source-based form is also unambiguous for Fusion IHD:

```r
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths",
      reference_id = "burnett_2022_fusion"
    )
  )
)
```

The preview output contains:

- `pollutant`
- `cause`
- `outcome`
- `action`
- `crf_id`
- `reference_id`
- `form`
- `selected_by_preset`
- `notes`

`action` is one of:

- `selected`: supplied by the starting preset;
- `added`: introduced by `add`;
- `removed`: excluded by `remove`; or
- `replaced`: introduced by `replace`.

Rows introduced by `add` or `replace` have
`selected_by_preset = NA` because they are explicit user choices rather than
members of the starting preset.

#### Exact-Operation Rules

- `add` is only for a slot absent from the current selection.
- `remove` requires `pollutant`, `cause`, and `outcome`.
- `replace` is only for a slot currently selected and not removed.
- `add` and `replace` accept either a direct `crf_id` or
  `pollutant + cause + outcome + reference_id`.
- A source-based request must match exactly one public Registry row.
- Use `crf_id` when one reference contains multiple CRFs for the same slot.

### `crfs_set(...)`

Creates a compute-ready `creahia_crf_set` from presets and optional exact
modifications.

Start from one preset:

```r
crfs <- crfs_set(
  presets = "fusion_adult_deaths_components"
)
```

Create the GEMM component selection with Fusion IHD substituted:

```r
crfs <- crfs_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)
```

Compose several operations after previewing the same request:

```r
set_spec <- list(
  presets = "gemm_adult_deaths_components",
  add = list(
    list(crf_id = "legacy_no2_asthma_1to18_incidence_v1")
  ),
  remove = list(
    list(
      pollutant = "PM25",
      cause = "LC",
      outcome = "Deaths"
    )
  ),
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)

preview <- do.call(preview_crf_set, set_spec)
crfs <- do.call(crfs_set, set_spec)
```

The returned object contains the full Registry rows needed by the downstream
PAF calculation, including `form`, `reference_id`, age
metadata, and calculation inputs.

Pass it to the existing HIA flow with the other required analysis inputs:

```r
hia <- compute_hia(
  conc_map = conc_map,
  species = "pm25",
  regions = regions,
  epi = epi,
  crfs = crfs
)
```

When `compute_hia()` or `compute_hia_paf()` receives a `crfs_set()` result, CRF
PAFs are calculated through the Registry path. `crfs_set()` itself does not run
HIA, change package data, or establish a new default model.

The `crf_compute` argument of `compute_hia()` and `compute_hia_paf()` is an
internal routing switch for maintainer diagnostics. It is not part of the
supported public interface and may be removed in Phase 2; pass a `crfs_set()`
result instead of setting it.

For `compute_hia()`, a supplied Set is complete: default legacy RR curves are
not calculated in addition to it. Explicit non-overlapping `rr_sources` can be
combined with the Set, while overlapping pollutant/cause/outcome slots are
rejected before PAF calculation.

If a calculation output contains an aggregate cause and its components in the
same pollutant/outcome group, `compute_hia()` uses `dc_groups` to mark component
rows `double_counted = TRUE`; exclude those rows from totals.

An explicit Set never skips a failing region silently. If any region fails, for
example because its exposure is outside a CRF's range or its EPI data are
missing, `compute_hia()` still checks every region and then stops with one
error that lists each failing scenario, region, `crf_id` and reason. No partial
results are returned. Fix those inputs or exclude the listed regions and run
again. Legacy-shaped calls keep their established behaviour of skipping a
failed region with a `Failed for region` warning.

### `crfs_preset(name)`

Resolves one preset directly to full Registry rows.

`name` is required; use `available_crf_presets()` to find public preset names.

```r
crfs <- crfs_preset("gbd2019_deaths")
```

This is useful when code needs the resolved rows but does not need composition
operations. For a compute-ready typed object, prefer `crfs_set()`.

### `resolve_crf_selection(selection)`

Resolves and validates an explicit selection table.

```r
selection <- tibble::tribble(
  ~pollutant, ~cause, ~outcome, ~crf_id,
  "PM25", "IHD", "Deaths", "fusion_pm25_ihd_adult_deaths_v1"
)

resolved <- resolve_crf_selection(selection)
```

The helper checks that every `crf_id` exists and that each
`pollutant + cause + outcome` slot selects only one CRF. Most user workflows
should begin with a preset instead of constructing this table manually.

### `validate_crf_selection(crfs, check_double_counting = FALSE)`

Validates an already resolved selection of full Registry rows. It checks that:

- the selection is not empty;
- each `crf_id` occurs once; and
- each `pollutant + cause + outcome` slot has only one CRF.

By default it does not reject overlapping causes. Set
`check_double_counting = TRUE` to also apply the package's double-counting
rules:

```r
crfs <- crfs_set(
  presets = "gemm_adult_deaths_components",
  replace = list(
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
  )
)

validate_crf_selection(crfs, check_double_counting = TRUE)
```

The function returns invisibly on success and errors with the conflicting IDs
or slots on failure. `resolve_crf_selection()`, `crfs_override()`, and
`crfs_set()` already call it internally. Calling it explicitly is most useful
after a lower-level manual edit to resolved Registry rows, or as a deliberate
final check in a reproducible HIA workflow.

### `crfs_override(crfs, pollutant, cause, outcome, crf_id)`

Replaces one exact slot in an already resolved Registry selection.

```r
crfs <- crfs_preset("gemm_adult_deaths_components")

crfs <- crfs_override(
  crfs,
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths",
  crf_id = "fusion_pm25_ihd_adult_deaths_v1"
)
```

This lower-level helper returns resolved Registry rows but does not add the
`creahia_crf_set` class. Prefer `preview_crf_set()` followed by `crfs_set()` for
normal analysis workflows because that path makes the proposed composition
easier to review.

## Interpretation and Safety Boundaries

### Replacement Is A Scientific Choice

The API verifies that a requested CRF exists, matches the stated slot, and is
unambiguous. It does not decide whether GEMM, Fusion, or GBD 2019 is the right
source for a study. Record the source choice and rationale alongside the HIA
configuration.

### Mixed-Source Sets Are Explicit

Replacing GEMM IHD with Fusion IHD creates a mixed-source Set. This is useful
for demonstrating and testing the override mechanism, and it may be appropriate
for a study with a documented rationale. The preview step is important because
the resulting Set no longer represents the unchanged packaged GEMM preset.

### Age Structure Matters

Two CRFs can share a pollutant, cause, outcome, and reference while differing
in age structure. For example, the Registry contains both a GEMM IHD `25+`
aggregate curve and a GEMM IHD adult split-age curve. Use `crf_id` whenever a
reference-based request would match more than one row.

### Aggregate And Component Mortality Must Not Be Combined

`NCD.LRI` overlaps its component causes. Choose either the aggregate
representation or the component representation for one model family; do not
combine both presets in the same HIA.

### Public Registry Is Not The Whole Legacy Surface

The available public rows now support real discovery, override, preview, and
compute examples. However, historical reproduction data and unresolved legacy
methodology are managed separately from public, opt-in Registry choices. Do not
infer support for an outcome or workflow merely because a related death CRF is
available.

## Validation Checklist

Before using a custom Set in an analysis:

1. inspect the starting preset with `describe_crf_preset()`;
2. use `crf_override_options()` to compare the same HIA slot;
3. inspect reference metadata with `available_crf_references()`;
4. preview the complete composition with `preview_crf_set()`;
5. check that aggregate and component causes do not overlap;
6. confirm the selected age structure and outcome match the study design; and
7. create the final typed object with `crfs_set()`.

For package development, run the focused selection tests:

```r
devtools::test(filter = "crf_select")
devtools::test(filter = "crf_fixture_isolation")
```

When changing GEMM or Fusion Registry data or presets, also run the relevant
end-to-end parity and fixed-fingerprint tests.


### How total inclusion is decided

`double_counted` is a calculation output, not a CRF property. At the beginning
of PAF calculation, the final CRF/RR selection is resolved once against the
shared cause coverage table. Selected ensembles take precedence over their
components within the same pollutant and outcome. All estimates remain
available for comparison; use `!double_counted` when summing impacts.

| Selection | Included in totals |
| --- | --- |
| TotCV alone | TotCV |
| IHD, Stroke, OthCV | All three components |
| TotCV, IHD, Stroke, OthCV | TotCV |
| TotResp, COPD, OthResp | TotResp |
| AllCause, TotCV, TotResp | AllCause |

TotResp means chronic respiratory diseases and does not include LRI. Editing
an inspected Set is supported: each new calculation rebuilds the decision.
Old CRF `double_counted` columns are ignored. The output stage verifies that
an expected covering ensemble exists for each excluded component, but never
silently changes the initial selection when a result is missing.

PAF tables returned by `compute_hia_paf()` carry the selection plan in the
`creahia.overlap_plan` attribute. Standalone impact calculation of an imported
PAF table without that attribute resolves inclusion from the available PAF
slots once at entry. Such a table cannot identify a selected cause that was
never exported.
