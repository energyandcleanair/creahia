# Provisional Historical Configuration Mapping

## Status

This document is a mapping specification, not a decision on public preset or
Set names. The identifiers beginning with `legacy_` and `provisional_` are
temporary planning IDs only.

## Purpose

Before comparing old and new fingerprints, define what “equivalent” means.
For each old calculation, the mapping must identify:

1. the old wrapper parameters;
2. the old CRF profile, if one was explicitly selected;
3. the old RR-source selection;
4. the epidemiological and population settings; and
5. the exact Registry rows and RR settings that the new calculation must use.

The mapping is deliberately split into two layers:

- **CRF profile**: rows loaded by old `get_crfs(crfs_version = ...)`;
- **RR configuration**: GBD/GEMM/Fusion RR sources selected by
  `calc_causes`, `gbd_causes`, or explicit `rr_sources`.

These layers must not be collapsed into a single preset name until their
scientific contents are verified.

## Terminology used here

### Registry configuration

A complete calculation specification, including the selected CRF rows and
other settings such as `epi_version` and `pop_year`.

### Provisional selection

A temporary description of the Registry rows that should be selected. It is
not yet a public preset or Set name.

### Preset and Set

A preset is a reusable file-backed CRF selection. A Set can compose presets and
apply workflow-specific additions, removals, or replacements. The eventual
historical configuration may use one preset, several presets, or a Set; this
document does not decide that yet.

## Evidence from the current code

### Old CRF profiles

`R/hia_data.R::get_crfs_versions()` routes the old profile names as follows:

| Temporary profile ID | Old argument | Source file | Active rows | Current mapping status |
| --- | --- | --- | ---: | --- |
| `legacy_profile_default` | `crfs_version = "default"` | `CRFs.csv` | 15 | Source rows known; Registry equivalent incomplete |
| `legacy_profile_c40` | `crfs_version = "C40"` | `CRFs_C40.csv` | 19 | Source rows known; Registry equivalent incomplete |
| `legacy_profile_krewski_south_africa` | `crfs_version = "Krewski-South Africa"` | `CRFs_Krewski_SouthAfrica.csv` | 21 | Source rows known; Registry equivalent incomplete |

The existing migration manifest confirms that these are 25 distinct CRF
definitions represented by 55 source-row memberships. The profile names in
this table are historical source labels, not proposed public API names.

### Current immutability-test behavior

`tests/testthat/test_results_immutability.R::get_fingerprint_bgd()` calls
`wrappers.compute_hia_two_images.default()` without passing `crfs_version`.
Because the wrapper default is `crfs_version = "default"`, the current
fingerprints implicitly use `legacy_profile_default`.

The current fingerprint test therefore covers RR/EPI/population variations
under the default CRF profile. It does not yet cover the C40 or
Krewski-South-Africa CRF profiles.

### Deterministic legacy CRF PAF baseline

`tests/testthat/test_legacy_default_crf_paf_fingerprint.R` now fixes a small
synthetic concentration/population input and asserts the 15 PAF rows produced
by `get_crfs("default")` against
`tests/testthat/fixtures/legacy_default_crf_paf_fingerprint.csv`.

This is a **legacy numerical baseline**, not approval of every legacy row's
scientific definition. It is intentionally independent of EPI and impact
calculation so a future Registry migration can first show that its CRF/PAF
math matches the old default rows. Full HIA fingerprints remain necessary to
validate the later EPI and impact-routing layers.

## Provisional mapping: current fingerprint configurations

These rows describe the old configurations already enumerated in
`test_results_immutability.R`. `target_selection_id` is intentionally not a
preset name.

| Mapping ID | Old RR specification | `epi_version` | `pop_year` | Old CRF profile | Provisional new target | Status |
| --- | --- | --- | ---: | --- | --- | --- |
| `legacy_default_gbd2019_only_2020` | `calc_causes = "GBD only"`; `gbd_causes` defaults to `"default"` | `gbd2019` | 2020 | `legacy_profile_default` | `provisional_default_profile_plus_gbd_only_gbd2019` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_gemm_gbd2019_2020` | `calc_causes = "GEMM and GBD"`; `gbd_causes` defaults to `"default"` | `gbd2019` | 2020 | `legacy_profile_default` | `provisional_default_profile_plus_gemm_gbd_gbd2019` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_gbd2021_only_2020` | `calc_causes = "GBD only"`; `gbd_causes` defaults to `"default"` | `gbd2021` | 2020 | `legacy_profile_default` | `provisional_default_profile_plus_gbd_only_gbd2021` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_gemm_gbd2021_2020` | `calc_causes = "GEMM and GBD"`; `gbd_causes` defaults to `"default"` | `gbd2021` | 2020 | `legacy_profile_default` | `provisional_default_profile_plus_gemm_gbd_gbd2021_2020` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_gemm_gbd2021_2021` | `calc_causes = "GEMM and GBD"`; `gbd_causes` defaults to `"default"` | `gbd2021` | 2021 | `legacy_profile_default` | `provisional_default_profile_plus_gemm_gbd_gbd2021_2021` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_fusion_gbd2021_2021` | `rr_sources = RR_FUSION` | `gbd2021` | 2021 | `legacy_profile_default` | `provisional_default_profile_plus_fusion_gbd2021_2021` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_gbd2023_gbd2021_2021` | `rr_sources = RR_GBD2023` | `gbd2021` | 2021 | `legacy_profile_default` | `provisional_default_profile_plus_gbd2023_gbd2021_2021` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_fusion_gbd2023_2023` | `rr_sources = RR_FUSION` | `gbd2023` | 2023 | `legacy_profile_default` | `provisional_default_profile_plus_fusion_gbd2023_2023` | RR mapping known; Registry CRF coverage incomplete |
| `legacy_default_gbd2023_gbd2023_2023` | `rr_sources = RR_GBD2023` | `gbd2023` | 2023 | `legacy_profile_default` | `provisional_default_profile_plus_gbd2023_gbd2023_2023` | RR mapping known; Registry CRF coverage incomplete |

### Important interpretation

For the first two rows, `calc_causes` is not itself a CRF profile selector.
The old code converts it into an RR-source/cause selection, while
`crfs_version` independently controls the legacy log-linear CRF table. This is
why the future mapping needs both a CRF selection and an RR configuration.

The conversion currently implemented in
`R/get_cause_source.R::convert_old_parameters_to_rr_sources()` gives the
following RR mapping:

```r
convert_old_parameters_to_rr_sources(
  calc_causes = "GEMM and GBD",
  gbd_causes = "default"
)
# RR_GEMM:     NCD.LRI, LRI, COPD, IHD, LC, Stroke
# RR_ORIGINAL: LRI.child, Diabetes
```

The code above is evidence of the old selection behavior, not a proposal to
keep this conversion function in the breaking-change public API.

## Provisional mapping: historical CRF profiles not covered by current fingerprints

| Mapping ID | Old CRF profile | Current fingerprint coverage | What must be added before comparison |
| --- | --- | --- | --- |
| `legacy_profile_default` | `CRFs.csv`, 15 rows | Implicitly used by current fingerprints, but not isolated as an explicit mapping input | Registry rows, verified references, and an explicit Registry selection |
| `legacy_profile_c40` | `CRFs_C40.csv`, 19 rows | Not covered by current `test_results_immutability.R` | Registry rows, Set composition decision, and a dedicated fingerprint scenario |
| `legacy_profile_krewski_south_africa` | `CRFs_Krewski_SouthAfrica.csv`, 21 rows | Not covered by current `test_results_immutability.R` | Registry rows, Set composition decision, and a dedicated fingerprint scenario |

The current `experimental_default` preset must not be used as the historical
target. It is a development/test preset and contains only a small subset of
the full historical collection.

## What is known and unknown

### Known now

- The old profile source files and their active row counts.
- The 25-definition/55-membership migration scope.
- The nine RR/EPI/population parameter combinations currently enumerated by
  the immutability test.
- The old conversion behavior for the `calc_causes`/`gbd_causes` combinations
  used by that test.
- The current fingerprint function implicitly uses the old `default` CRF
  profile.

### Not decided yet

- Formal public preset names.
- Whether the historical default is one preset or a Set composed of several
  outcome-family presets.
- Exact contents of the historical profile selections after Registry
  migration.
- Historical GBD/IER-only versus GBD-plus-GEMM composition at the public
  Registry boundary.
- Reference-specific aggregate and double-counting treatment for L12-L25.

## Machine-readable configuration inventory

The nine current fingerprint configurations are now stored in the test-only
fixture `tests/testthat/fixtures/historical_fingerprint_configurations.csv`.
`tests/testthat/helper-historical-fingerprint-configurations.R` reads the CSV
with declared column types, and
`tests/testthat/test_historical_fingerprint_configurations.R` verifies that it
still represents the current legacy test matrix.

Every current row has `mapping_status = "legacy_defined"`. This means the old
configuration is known, but its Registry target is not ready. Accordingly,
`target_selection_id` is empty and `blocking_reason` is required. The fixture
must not substitute `experimental_default` for a historical target.

## Planned fingerprint dispatcher

The next code slice should create a machine-readable mapping object rather than
hard-coding the table in the fingerprint test. Conceptually:

```r
historical_configurations <- tibble::tribble(
  ~mapping_id, ~legacy_crfs_version, ~epi_version, ~pop_year, ~rr_mode,
  "legacy_default_gbd2019_only_2020", "default", "gbd2019", 2020, "gbd_only",
  "legacy_default_gemm_gbd2019_2020", "default", "gbd2019", 2020, "gemm_gbd"
)
```

This object is a test specification. Each row says which old configuration to
run and which new target mapping must eventually be supplied. It does not
calculate HIA results by itself.

The eventual fingerprint dispatcher will use the mapping row approximately as
follows:

```r
run_fingerprint_configuration <- function(config, ref) {
  if (is_old_release(ref)) {
    run_legacy_fingerprint(config)
  } else {
    run_registry_fingerprint(
      registry_selection = resolve_provisional_target(config$mapping_id),
      epi_version = config$epi_version,
      pop_year = config$pop_year,
      rr_mode = config$rr_mode
    )
  }
}
```

Function roles:

- `is_old_release()` decides whether to use historical arguments or the new
  Registry route.
- `run_legacy_fingerprint()` preserves the old call contract for old package
  versions.
- `resolve_provisional_target()` translates the mapping ID into explicit
  Registry rows; it must fail if the mapping is not complete.
- `run_registry_fingerprint()` runs the new path with the same exposure,
  population, epidemiology, and RR assumptions.

The important safety rule is that an incomplete mapping must produce an
explicit “not ready” failure. It must not silently substitute
`experimental_default` or another convenient preset.

## Next smallest implementation slice

1. Populate `target_selection_id` with explicit CRF IDs once each row has been
   verified; keep public preset/Set names blank for now.
2. Add a readiness test: every mapping marked `registry_ready` must have one
   complete target selection and must not contain an internal fixture.
3. Only after that, adapt `generate_fingerprint()` for the first fully mapped
   configuration.

## Current blockers

- The Registry does not yet contain the complete historical CRF collection.
- Several reference IDs and aggregate-cause rules still require verification.
- C40 and Krewski-South-Africa are not represented in the current fingerprint
  scenarios.
- The public preset/Set naming and composition remain intentionally open.
