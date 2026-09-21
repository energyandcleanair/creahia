# Step 1: CRF Registry and Validation

## Status

Completed as the first foundation slice.

## Goal

Create a central registry for concentration-response functions so future code can
select CRFs by metadata instead of hard-coded paths and source-specific logic.

## Implemented Files

- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/references.csv`
- `R/crf_registry.R`
- `tests/testthat/test_crf_registry.R`

## Registry Shape

Required registry columns:

- `crf_id`
- `pollutant`
- `cause`
- `outcome`
- `age_low`
- `age_high`
- `region_applicability`
- `form`
- `rr_central`
- `rr_low`
- `rr_high`
- `conc_change`
- `counterfact`
- `unit`
- `units_multiplier`
- `data_path`
- `reference_id`
- `notes`

Required reference columns:

- `reference_id`
- `author`
- `year`
- `title`
- `journal`
- `volume`
- `pages`
- `doi`
- `url`
- `notes`

## Validation Rules

The implemented validators check:

- all required columns are present
- `crf_id` values are unique
- `reference_id` values exist in the references table
- `form` is either the log-linear form constant or the tabular form constant
- log-linear rows have non-missing numeric RR/concentration fields
- log-linear RR values are positive and ordered as
  `rr_low <= rr_central <= rr_high`
- log-linear concentration fields are non-negative where appropriate
- tabular rows have a non-missing `data_path`
- tabular `data_path` files exist under `inst/extdata`

## Current Registry Rows

- `legacy_no2_asthma_1to18_incidence_v1`
- `legacy_no2_ncdlri_deaths_v1`
- `test_tabular_pm25_ihd_deaths_v1`
- `gemm_pm25_ihd_25plus_deaths_v1`

## Fixture Policy

`test_tabular_pm25_ihd_deaths_v1` is intentionally a fixture row. It exists to
prove that the registry can validate a tabular CRF path before the full tabular
migration is complete. It should not be treated as a real migrated CRF.

## Verification

Run:

```r
devtools::test(filter = "crf_registry")
```

Expected coverage:

- references load and validate
- registry loads and validates
- registry/reference IDs match
- log-linear numeric fields are complete and valid
- invalid forms fail validation
- unknown references fail validation

## Follow-up

- Keep `log_linear` as the canonical stored value for log-linear CRFs. Do not
  migrate it to `log-linear`.
- Use constants everywhere `form` is filtered or compared. Do not hard-code
  `"log_linear"` or `"tabular"` in validation or dispatch code.
- Keep strict validation for the `form` column so spelling mistakes fail early.
- Add stricter tabular file content validation when more tabular CRFs are
  migrated.
