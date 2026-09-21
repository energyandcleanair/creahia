# Step 3: Apply Helpers and Log-linear Migration

## Status

Complete for helper-level parity. The helper structure exists, the log-linear
path has a direct legacy parity test, and it is ready to support the next
registry-to-compute integration slice.

## Goal

Create one `apply_crf()` interface that can dispatch by registry row:

- `form == "tabular"` uses a tabular RR table
- `form == "log_linear"` uses log-linear RR parameters from the registry

Both paths should return the same standard PAF row shape:

- `pollutant`
- `cause`
- `outcome`
- `region_id`
- `low`
- `central`
- `high`

## Implemented Files

- `R/crf_apply.R`
- `tests/testthat/test_crf_apply.R`

## Implemented Functions

- `apply_crf()`
- `apply_crf_tabular()`
- `load_crf_tabular()`
- `get_crf_age_weights()`
- `get_hazard_ratio_tabular()`
- `apply_crf_log_linear()`
- `calculate_effective_conc_change()`
- `calculate_log_linear_paf()`

## Tabular Path

The tabular helper:

1. loads the tabular CRF data from `data_path`
2. extracts and validates the age groups
3. gets age weights from long-format EPI data
4. interpolates RR values with `approx()`
5. delegates weighted PAF aggregation to `get_paf_from_rr_lauri()`
6. returns the standard PAF row shape

The current test compares `apply_crf_tabular()` against the legacy
`country_paf_perm()` path for a one-age fixture.

## Log-linear Path

The intended log-linear helper:

1. computes the weighted source concentration difference between baseline and
   scenario concentration
2. applies the closed-form log-linear PAF calculation:
   `1 - exp(-log(rr) * concentration_change_actual / concentration_change_ref)`
3. returns the standard PAF row shape

## Known Review Point

Before this step is considered complete, review `calculate_effective_conc_change()`
against the legacy `compute_hia_paf_crfs()` logic. The helper should take and use
the baseline concentrations, scenario concentrations, and population weights
explicitly.

Current implementation status:

- `calculate_effective_conc_change()` takes baseline concentrations, scenario
  concentrations, population weights, counterfactual, and unit multiplier
  explicitly.
- `apply_crf_log_linear()` returns the standard PAF row shape.
- `tests/testthat/test_crf_apply.R` covers the closed-form log-linear formula
  and a simple weighted concentration-delta case.
- `tests/testthat/test_crf_apply.R` also covers parameter routing from
  `apply_crf_log_linear()` into the internal PAF calculator.
- `tests/testthat/test_crf_apply.R` includes a direct numerical parity test
  against legacy `compute_hia_paf_crfs()` for one NO2 log-linear CRF.

## Naming Cleanup

Mentor feedback asks that concentration-change names be explicit:

- rename `conc_ref`/`con_ref` style variables when they mean the reference
  concentration change, for example `conc_change`
- rename `source_conc`/`source_cons` style variables when they mean the actual
  concentration change, for example `conc_change_actual`

The key rule is that names should distinguish concentration deltas from absolute
concentration values.

Current implementation status:

- The new helper uses `effective_conc_change`.
- The legacy `compute_hia_paf_crfs()` path still uses `source_concs` for the
  actual concentration delta and still contains the old NRT branch.

## Legacy Cleanup

When this path replaces the old CRF calculation, remove the historical NRT
pollutant-name behavior. Pollutant names should stay pure; counterfactual logic
should come from registry metadata, not from checking whether a species name
contains `_NRT`.

## Verification

Run:

```r
devtools::test(filter = "crf_apply")
```

Expected coverage:

- tabular helper matches the legacy PAF helper for the one-age fixture
- tabular interpolation returns expected low/central/high values
- log-linear PAF formula matches the closed-form attributable fraction
- log-linear helper returns the standard PAF row shape
- log-linear helper routes `effective_conc_change` to the internal PAF
  calculator
- log-linear helper matches legacy `compute_hia_paf_crfs()` for one NO2
  log-linear CRF

## Follow-up

- Wire `apply_crf()` into a new registry-driven compute path only after the
  helper-level parity tests are stable.
- After parity is established, remove the old PAF path and old argument
  translation logic rather than maintaining backward compatibility.

## 2026-08-17: Complete legacy migration contract

The helper and compute wiring are ready, but the next data migration must not
add only a convenient subset of legacy log-linear CRFs. Mentor feedback requires
one complete active legacy scope, documented in
`08-legacy-crf-migration-manifest.md`.

Before registry rows or presets are added:

1. Account for all 55 active legacy source rows (15 historical-default, 19 C40,
   and 21 Krewski-South Africa), including their profile membership.
2. Recover a real reference row for every legacy CRF. A blank legacy
   `reference` field is a provenance task, not permission to assign a generic
   `legacy_default_crfs` reference permanently.
3. Preserve legacy numeric parameters and age applicability, while explicitly
   resolving rows whose historical table did not supply age bounds.
4. Verify aggregate/component behaviour for NCD.LRI, TotResp, TotCV, and
   AllCause against the relevant reference before setting target
   `double_counted` metadata.
5. Add data-coverage and numeric parity tests that prove every manifest CRF is
   present exactly once in the intended preset/Set composition.

The planned outcome-family presets are `asthma_original`, `birth_original`, and
`work_absence_original`. Mortality CRFs require a provisional explicit grouping
until their source provenance is complete. C40 and Krewski-South Africa are
workflow Sets that compose those reusable presets and their profile-specific
rows; they are not a substitute for provenance.
