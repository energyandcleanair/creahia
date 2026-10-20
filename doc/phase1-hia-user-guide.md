# CREAHIA Phase 1: HIA User Guide

- Updated: 2026-10-20
- Scope: Phase 1 - Compatibility-Preserving Registry Rollout (version 0.8.0)

## What This Update Changes

Phase 1 makes the CRFs, RR sources, EPI data, and literature references used in
an HIA easier to inspect and confirm, **without requiring existing HIA projects
to rewrite their workflows**.

For most users, the main changes are:

1. existing wrappers, arguments, and output formats continue to work;
2. the package resolves supported existing configurations into an explicit
   Registry method Set and runs them through the Registry-driven calculation
   route;
3. users can inspect the methods, components, and references selected before
   running an HIA;
4. users can start from a Preset, preview changes, and create an explicit
   `crfs_set()`;
5. the package prevents two CRFs from occupying the same HIA slot and can apply
   strict double-counting checks to aggregate and component outcomes; and
6. the old calculation route remains temporarily available to maintainers for
   investigating a suspected Phase 1 software regression.

Phase 1 improves method selection, provenance, and traceability. Passing
numerical reproduction or compatibility tests does not mean that a method has
received scientific approval.

## Installation and Version Check

Phase 1 is released as `creahia` 0.8.0. After the approved pull request is
merged and the `0.8.0` tag has been created, install that tagged version on
your own computer:

```r
remotes::install_github(
  "energyandcleanair/creahia@0.8.0",
  upgrade = "never"
)
```

After installation:

1. Restart R.
2. Confirm that the package loads successfully.
3. Check the installed version and location.

```r
library(creahia)

packageVersion("creahia")
find.package("creahia")
```

`packageVersion("creahia")` should report `0.8.0`. The path returned by
`find.package("creahia")` is the library used on your computer; it is useful
when diagnosing a local installation. When reporting an issue, include both 
outputs and the relevant workflow.

## What Does Not Need to Change in Existing Workflows?

Existing HIA projects can continue using the same wrappers and their existing
`crfs_version`, `epi_version`, `calc_causes`, `gbd_causes`, and RR source
settings. For example:

```r
wrappers.compute_hia_two_images(
  ...,
  crfs_version = "C40",
  epi_version = "C40",
  calc_causes = causes_to_include
)
```

For supported configurations, Phase 1 internally translates legacy-shaped
inputs into a Registry Set. Users continue to receive compatible return values,
columns, and saved outputs.

Phase 1 also does not automatically change:

- the concentration, population, or EPI inputs used by a project;
- the public arguments of existing wrappers;
- the fact that the CRF profile, RR source, EPI version, and population year
  are separate decisions;
- responsibility for the scientific interpretation of outputs; or
- the rule that overlapping aggregate and component outcomes must not be
  summed together.

## Four Main Concepts in the New Architecture

### Registry

The Registry is the catalogue of available CRFs. Each CRF has a stable
`crf_id` and records its pollutant, cause, outcome, age range, curve form,
calculation inputs, and `reference_id`.

### Reference

The Reference table stores source information such as author, year, title,
DOI, and URL. Method definitions and literature sources are stored separately
and linked through `reference_id`.

### Preset

A Preset is a reusable starting selection. Each row maps one HIA slot:

```text
pollutant + cause + outcome
```

to one `crf_id`. A Preset is a starting point; it does not mean that every
analysis should use the same selection.

### Set

`crfs_set()` represents the complete method Set actually used in one analysis.
It resolves a Preset and any explicit `add`, `remove`, or `replace` operations
into complete Registry rows for use by `compute_hia()`.

## Recommended Workflow 1: Inspect an Existing Configuration Without Rewriting the Project

`inspect_hia_configuration()` accepts the same settings as an existing wrapper
and shows the planned route, components, method Set, and references before the
calculation is run. It does not run an HIA or change package data or the
original settings.

```r
causes_to_include <- c(
  "Stroke_YLDs",
  "Diabetes_YLDs",
  "COPD_YLDs"
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

The printed summary shows:

- `Route`: whether the configuration is expected to use `registry`, `legacy`,
  or remain `unchanged`;
- whether it is ready for Registry routing;
- the canonical input contract;
- the number of CRF, RR, and EPI components; and
- the number of references.

Three levels of detail are available when further review is needed:

```r
# Review CRF, RR and EPI components and their status.
inspection$components |>
  dplyr::select(
    component_type,
    source,
    cause,
    outcome,
    age_contract,
    lifecycle,
    routing_status,
    scientific_review_status
  )

# Review the exact methods selected for calculation.
inspection$registry_set |>
  dplyr::select(
    crf_id,
    pollutant,
    cause,
    outcome,
    form,
    reference_id
  )

# Review the literature sources linked to the selected methods.
inspection$references |>
  dplyr::select(reference_id, author, year, title, doi, url)
```

`routing_status` reports whether the engineering route is ready.
`scientific_review_status` is a separate field. A definition marked
`historical_reproduction` or `not_reviewed` can support compatibility
reproduction without being recommended as a formal scientific default.

## Recommended Workflow 2: Find a Suitable Preset and CRF

First list the available Presets, then inspect the CRFs selected by one Preset:

```r
# List available starting selections.
available_crf_presets()

# Inspect methods and references selected by one preset.
describe_crf_preset("gemm_adult_deaths_components")
```

To compare alternatives for one HIA slot:

```r
crf_override_options(
  presets = "gemm_adult_deaths_components",
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)
```

`selected = TRUE` identifies the current Preset selection. `selected = FALSE`
identifies public alternatives for the same slot. An alternative being
available does not mean that it is more suitable or scientifically preferred;
the choice must still be based on the purpose and sources of the analysis.

The Registry can also be searched directly without starting from a Preset:

```r
search_crf_registry(
  pollutant = "PM25",
  cause = "IHD",
  outcome = "Deaths"
)

available_crf_references()
```

## Recommended Workflow 3: Preview Before Creating a Custom Set

`add`, `remove`, and `replace` have different purposes:

- `add`: add an HIA slot that is not currently selected;
- `remove`: remove a currently selected slot; and
- `replace`: replace the current CRF for a slot with another CRF.

The following example starts from the GEMM adult component Preset:

```r
add <- list(
  list(crf_id = "legacy_no2_asthma_1to18_incidence_v1")
)

remove <- list(
  list(
    pollutant = "PM25",
    cause = "LRI",
    outcome = "Deaths"
  )
)

replace <- list(
  list(crf_id = "fusion_pm25_ihd_adult_deaths_v1")
)

# Preview the full selection before creating a compute-ready Set.
preview_crf_set(
  presets = "gemm_adult_deaths_components",
  add = add,
  remove = remove,
  replace = replace
)
```

After reviewing the preview, create the Set with the same specification:

```r
crfs <- crfs_set(
  presets = "gemm_adult_deaths_components",
  add = add,
  remove = remove,
  replace = replace
)

# Confirm one CRF per slot and reject double-counting conflicts.
validate_crf_selection(
  crfs,
  check_double_counting = TRUE
)
```

The resulting `crfs` object can then be passed to the existing HIA compute
flow:

```r
hia <- compute_hia(
  conc_map = conc_map,
  species = "pm25",
  regions = regions,
  epi = epi,
  crfs = crfs
)
```

Creating a `crfs_set()` does not modify the Registry or package data, and it
does not run an HIA by itself.

## Selection Rules and Double-Counting

### One HIA Slot Can Use Only One CRF

Two CRFs cannot occupy the same `pollutant + cause + outcome` slot. If Presets
conflict, or if `add` is used for a slot that is already selected, the preview
stops and identifies the conflict. Use `replace` to change an existing slot.

### Aggregate and Component Results May Be Viewed Together but Must Not Be Summed Together

For example, the `NCD.LRI` aggregate overlaps with component causes including
IHD, Stroke, COPD, LC, and LRI. The system can retain both types of result for
comparison, but they must not all be added together.

Use strict validation for a formal method Set:

```r
validate_crf_selection(
  crfs,
  check_double_counting = TRUE
)
```

If an aggregate and its components are both present, strict validation stops
and identifies the conflict group. Do not remove or ignore `double_counted`
information simply to obtain a total.

## Temporary Calculation-Route Fallback

Under normal use, `creahia.hia_compatibility_routing` does not need to be set.
When the option is unset, supported existing workflows use the Phase 1 Registry
route:

```r
# Normal use: leave the routing option unset.
options(creahia.hia_compatibility_routing = NULL)
```

When a maintainer is investigating a software regression suspected to come
from Phase 1 routing, the old calculation route can be restored temporarily in
a controlled R session:

```r
# Diagnostic fallback only; do not add this to normal project templates.
options(creahia.hia_compatibility_routing = FALSE)
```

When using the fallback:

- use it only to compare a suspected Phase 1 software regression;
- add it only to the affected workflow or diagnostic session;
- record the workflow, reason, package version or commit, and comparison
  result;
- do not use it to bypass methodology review of CRF, RR, EPI, disease, age,
  counterfactual, uncertainty, or double-counting decisions; and
- after the comparison, return to normal routing with
  `options(creahia.hia_compatibility_routing = NULL)` or by restarting the R
  session.

This option is not a package rollback. It cannot repair an installation
failure, missing dependency, missing file, external-data problem, or
incompatible public interface. Those problems require fixing the environment
or reinstalling a known earlier package version.

An analysis that explicitly supplies a `crfs_set()` continues to use its
Registry selection directly. Do not treat `FALSE` as a routine switch between
scientific methods.

## Important Points When Using the Results

- Reproducing the same numerical results for an existing workflow demonstrates
  software compatibility; it does not provide new scientific approval for a
  historical method.
- The CRF profile, RR source, EPI version, and population year are four
  separate decisions. One must not silently determine another.
- A `Preset` is a starting point; `crfs_set()` records the final Set selected
  for an analysis.
- A search or alternative result means that a method is available, not that it
  is recommended.
- Aggregate and component results may be viewed together, but they must not be
  summed without an explicit overlap review.
- Phase 1 preserves the existing public workflow. A Registry-first public API
  belongs to the future Phase 2 and is outside the scope of this update.

## Further Details

This document is a quick guide for HIA users. For complete function details,
practical workflows, exact `add`/`remove`/`replace` rules, real Registry
examples, and validation behaviour, see:

- [CRF Registry Discovery and Selection](crf-registry-selection.md)
