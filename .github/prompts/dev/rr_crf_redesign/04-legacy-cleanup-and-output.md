# Step 4: Legacy Cleanup and Main Output Shape

## Status

Planned.

## Goal

After the registry-driven CRF path is validated numerically, remove old
compatibility surfaces and make the main output more useful for reporting.

## Scope

This step covers:

- removing legacy argument translation for old parameters
- migrating repository workflow templates and examples to Registry inputs
- removing NRT pollutant-name parsing
- removing `.region` branching
- returning references alongside computed results

## Deprecated Inputs to Remove

The future breaking-change version should remove conversion or compatibility
logic for:

- `C_causes`
- `GBD_causes`
- `RR_sources`
- `CRF_version`

Users should select CRFs through the new CRF registry system instead.

## NRT Cleanup

Remove historical code that checks whether pollutant/species names contain
`_NRT` and then changes counterfactual behavior.

New rule:

- pollutant names should be pure identifiers
- counterfactual values should come from CRF registry metadata
- any non-risk-threshold concept should be represented as explicit data, not a
  naming convention

## `.region` Cleanup

Remove `.region` and related branching that chooses whether GEMM data includes
China.

New rule:

- use the version that includes China by default
- avoid keeping parallel branches unless a current data requirement proves they
  are still needed

## Main Return Shape

The main wrapper should return a list rather than only a data frame.

Proposed shape:

```r
list(
  results = paf_or_hia_results,
  references = references_table
)
```

The references table should contain the CRF references actually used in the run,
so users can copy the table into reports.

## Validation Needed

Before changing the wrapper output:

- complete the version-aware fingerprint/non-regression work in Step 6 for the
  historical configurations covered by the cleanup
- identify which wrapper should own the final return shape, likely
  `compute_hia()` or the nearest reporting-facing wrapper
- add tests for the list structure
- add tests that references are filtered to the selected CRFs
- update examples or downstream callers that expect a bare data frame

Already completed before this step:

- registry-vs-legacy parity tests include not-zero/not-`NA` assertions
- legacy log-linear CRF `epi_version` behavior has been clarified:
  `compute_hia_paf_crfs()` does not read EPI data or do age-weighting
- tabular RR/CRF calculations remain separate because they do use EPI age
  weights

## Implementation Timing

Do this after:

1. registry selection is stable
2. `apply_crf()` is wired into the compute path
3. parity tests cover at least one old log-linear and one tabular CRF
4. parity tests include non-empty-result guards
5. the legacy log-linear CRF `epi_version` behavior is understood
6. equivalent Registry configurations pass the historical fingerprint tests,
   with any differences explicitly authorised and documented

Items 1-5 are complete for the current optional registry compute path. The next
choice is whether to promote registry-driven CRF PAF calculation to the default
before changing the main wrapper output shape.

This avoids changing the public output shape while the internal CRF selection
contract is still moving.
