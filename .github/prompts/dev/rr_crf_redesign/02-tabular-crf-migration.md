# Step 2: First Tabular CRF Migration

## Status

Completed for the first narrow GEMM slice.

## Goal

Migrate one real tabular RR curve into the new CRF registry structure without
changing the main compute flow yet.

The selected first slice is:

- pollutant: `PM25`
- cause: `IHD`
- outcome: `Deaths`
- age band: `25+`
- source: GEMM
- source file: `inst/extdata/rr/processed/rr_gemm.csv`

## Implemented Files

- `R/compile_crfs_gemm.R`
- `R/compile_crfs_utils.R`
- `inst/extdata/crf/tabular/gemm_pm25_ihd_25plus_deaths_v1.csv`
- `tests/testthat/test_crf_gemm_migration.R`
- registry row in `inst/extdata/crf/registry.csv`
- reference row in `inst/extdata/crf/references.csv`

## Implemented Functions

- `compile_gemm_ihd_25plus_deaths()`
- `validate_compiled_gemm_ihd()`
- `gemm_reference_row()`
- `upsert_crf_registry_row()`
- `upsert_crf_reference_row()`

Note: the upsert helpers now live in `R/compile_crfs_utils.R`. The duplicate
GEMM-local definitions have been removed from `R/compile_crfs_gemm.R`, and the
GEMM compiler calls the shared helpers when registry/reference updates are
requested.

## Output Contract

The migrated tabular CRF file has these columns:

- `exposure`
- `age`
- `low`
- `central`
- `high`

The compiler extracts rows from the legacy GEMM file where:

- `cause == "IHD"`
- `age == "25+"`
- `source == "gemm"`

Then it sorts by `exposure` and writes the new CRF table.

## Validation Rules

The compiler validation checks:

- required columns are present
- the compiled table is non-empty
- exposure values are not missing
- exposure values are sorted
- RR values are positive
- RR values are ordered as `low <= central <= high`

## Verification

Run:

```r
devtools::test(filter = "crf_gemm_migration")
```

Expected behavior:

- the migrated file exactly matches the same filtered/sorted rows from
  `inst/extdata/rr/processed/rr_gemm.csv`

## Design Decision

The first migration intentionally stays narrow. It proves the registry/data-file
pattern with one important CRF before expanding to all GEMM causes and age
groups.

## Follow-up

- Generalize the GEMM compiler so it can process the full GEMM dataset in one
  deterministic run, not only the IHD 25+ deaths slice.
- After the full GEMM compiler is stable, use the same pattern for Fusion and
  GBD compilers.
- Avoid updating registry/reference files by blind append; keep upsert behavior.
- Keep migrated files deterministic so tests can compare exact data.
