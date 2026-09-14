# Step 6: Parity Testing and EPI Version Trace

## Status

Implemented for the current registry fixtures and optional compute path.

## Goal

Make registry-vs-legacy parity tests trustworthy before using them to justify
compute-path replacement.

This step had two parts:

- strengthen parity tests so they cannot pass on meaningless all-zero or `NA`
  outputs
- trace how `epi_version` flows through legacy and registry paths

## HIA Meaning

PAF results depend on both:

- the concentration-response function, which converts exposure change into risk
- the EPI age weights, which determine how age-specific risks are aggregated

If population and concentration grids do not align, both old and new code can
return zero. That would make an equality test pass even though the test did not
exercise a meaningful HIA calculation.

If the new registry path passes `epi_version` explicitly but the legacy path
silently falls back to a default, parity may depend on hidden assumptions.

## Testing Enhancement

For tests comparing registry and legacy PAF results:

- keep the equality assertion
- also assert that compared values are not `NA`
- also assert that compared values are not all exactly zero

Example intent:

```r
expect_equal(new_values, legacy_values, tolerance = 1e-12)
expect_true(all(!is.na(new_values)))
expect_true(any(new_values != 0))
```

For data-frame outputs, select the numeric PAF columns first:

- `low`
- `central`
- `high`

Then apply the same not-`NA` and not-all-zero checks.

## Confirmed `epi_version` behavior

The trace confirmed:

- where `apply_crf_tabular()` passes `epi_version`
- where `country_paf_perm()` calls `get_age_weights()`
- whether the legacy path can receive `epi_version` from a caller
- whether legacy code relies on `epi_version = "default"` inside a lower-level
  function

## Output of This Step

This step produced:

- updated parity tests with not-empty-result guards
- a short note in the step file explaining legacy `epi_version` behavior
- a decision on where the registry path should accept/default `epi_version`

## Verification

Run focused tests after implementation:

```r
devtools::test(filter = "crf_apply")
devtools::test(filter = "compute_hia_paf")
```

Adjust the exact filter names based on where the parity tests live.

## Verification completed

- `tests/testthat/test_compute_hia_paf.R` now checks registry-vs-legacy parity for
  one log-linear CRF and one tabular CRF.
- The parity checks assert matching output structure and values, plus non-`NA`
  and non-all-zero guards.
- The `epi_version` trace confirmed that legacy log-linear CRFs do not read EPI
  age weights, while tabular CRFs do use EPI age weights through
  `apply_crf_tabular()`.

## Follow-up

The registry-selected CRF path is now wired into `compute_hia()` and
`compute_hia_paf()` as an opt-in path. The end-to-end
`tests/testthat/test_registry_smoke.R` smoke test now covers that boundary while
keeping the legacy default unchanged.

## Version-aware fingerprint non-regression

### Status

Planned from mentor feedback on 2026-08-24.

### Goal

Protect the breaking change against accidental numerical drift. This test is
about immutability of HIA/PAF results under equivalent scientific settings; it
is not a test that old parameter names remain accepted.

### Required behavior

Adapt `tests/testthat/test_results_immutability.R` so fingerprint generation
dispatches by package version:

- historical package versions use historical arguments such as `calc_causes`
  and `rr_sources`;
- the new package version uses the corresponding Registry preset or Set;
- the comparison normalizes only presentation/schema differences and compares
  central PAF estimates for equivalent configurations;
- missing result rows and unexpected numerical changes fail the test; and
- a known, reviewed bug fix may be listed as an authorised difference with a
  short reason and narrow matching scope.

The first implementation should reuse the existing Bangladesh deterministic
fixture and representative GBD2019, GBD2021, GBD2023, Fusion, and GEMM/GBD
configurations where the Registry has an explicit equivalent. Do not declare a
difference authorised merely because the new code is structurally different.

### HIA meaning

The Registry changes how a CRF is selected and applied, but it must not silently
change the attributable burden calculation. For equivalent exposure,
population, epidemiological version, and risk model, the PAF is the numerical
contract between the old and new implementations.

### Validation

Run the focused immutability test after dispatch and parameter mapping are
implemented. Before changing snapshots, inspect every difference and record
whether it is an expected scientific/data correction or an unintended break.

### Foundation completed

- `tests/testthat/fixtures/historical_fingerprint_configurations.csv` records
  the nine historical RR/EPI/population configurations currently hard-coded in
  `test_results_immutability.R`.
- `tests/testthat/test_historical_fingerprint_configurations.R` checks their
  IDs and legacy parameters, and requires every not-yet-migrated mapping to
  retain an empty Registry target plus a specific blocking reason.
- No mapping is Registry-ready yet. The fixture deliberately rejects using
  `experimental_default` as a substitute historical target.
