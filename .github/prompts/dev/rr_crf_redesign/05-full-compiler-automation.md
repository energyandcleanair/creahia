# Step 5: Full Compiler Automation

## Status

Planned, with shared utility extraction complete for the current GEMM compiler.

## Goal

Move from one hand-selected GEMM migration to repeatable compiler scripts that
can regenerate registry rows, reference rows, and tabular CRF files for complete
source datasets.

## First Target: GEMM

Upgrade the current GEMM compiler from:

- one disease/outcome/age slice

to:

- the full GEMM dataset that should be represented in the registry

The output should remain deterministic so tests can compare generated files with
source data.

## Shared Utilities

Reusable compiler helpers now exist in:

- `R/compile_crfs_utils.R`

Implemented shared helpers:

- `upsert_crf_registry_row()`
- `upsert_crf_reference_row()`

Current cleanup status:

- duplicate definitions of these helpers have been removed from
  `R/compile_crfs_gemm.R`
- source-specific compilers should call the shared helpers rather than defining
  their own registry/reference upsert functions

Expected future shared helpers:

- common path handling
- common validation wrappers if repeated across compilers

## Later Targets

After GEMM is stable:

- add Fusion compiler support
- add GBD compiler support

Each new source should follow the same pattern:

1. read source data
2. normalize to registry/data-file contracts
3. validate
4. write deterministic files
5. upsert registry/reference rows
6. test exact expected output

## Tests

Add tests that check:

- generated files are deterministic
- registry rows are valid
- references rows are valid
- source-specific filters do not silently drop required rows
- all generated `data_path` files exist
