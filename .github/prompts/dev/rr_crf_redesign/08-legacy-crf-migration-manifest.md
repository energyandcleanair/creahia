# Legacy CRF Migration Manifest

## Purpose and status

This is the implementation contract for the complete legacy log-linear CRF
migration directed by mentor feedback on 2026-08-17. It is a planning manifest,
not yet a registry import and not a claim that all references have been
recovered.

Each manifest entry is one distinct legacy CRF definition. The **profiles**
column records every active historical source-row membership, so the table
accounts for all 55 rows currently routed by `R/hia_data.R` without duplicating
an identical CRF in the future registry:

| Active historical profile | Source table | Rows | Manifest accounting |
| --- | --- | ---: | --- |
| `historical_default` | `CRFs.csv` | 15 | 11 shared rows + 4 default-only rows |
| `c40` | `CRFs_C40.csv` | 19 | 11 shared rows + 8 C40/South-Africa rows |
| `krewski_south_africa` | `CRFs_Krewski_SouthAfrica.csv` | 21 | 11 shared rows + 8 C40/South-Africa rows + 2 South-Africa-only rows |
| **Total source rows** |  | **55** | **25 distinct CRF definitions** |

`historical_default`, `c40`, and `krewski_south_africa` in this file are planned
Set identifiers, not formal public defaults or scientific-source claims. The
historical GBD/IER-only versus GBD/IER-plus-GEMM choice is a separate RR-source
layer and is deliberately not encoded as EPI versioning.

## Required migration rules

- Preserve the shown RR parameters, counterfactual, unit, and multiplier.
- Recover full bibliographic references for every blank legacy `reference`
  value. Do not use `legacy_default_crfs` as a permanent substitute for the
  original evidence source.
- `CRFs.csv` omits age bounds. Where an identical C40/South-Africa definition
  supplies them, validate that they are the intended bounds before reuse. The
  four default-only NCD.LRI rows have no such corroborating age metadata.
- Treat the `double_counted` column as source evidence, not a universal cause
  taxonomy. Reference-dependent aggregate rows need explicit verification before
  target registry flags and `dc_groups.csv` relationships are finalised.
- The expected preset names below are planning names. Only
  `asthma_original`, `birth_original`, and `work_absence_original` have mentor
  support; `mortality_original` remains provisional.

## Row-level manifest

`RR; delta/cf; unit x multiplier` retains the full numeric legacy CRF contract.
`Reference plan` is intentionally explicit about missing provenance.

| ID | CRF | RR; delta/cf; unit x multiplier | Profiles (source rows) | Legacy reference / reference plan | Outcome family | Double-counting plan | Expected preset | Expected Set membership |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| L01 | `NO2 / Asthma.1to18 / AsthmaIncidence` | `1.10/1.26/1.37; 10/2; ppb x 0.532` | historical_default, c40, krewski_south_africa (3) | blank / recover source | asthma | legacy `FALSE`; no aggregate relation recorded | `asthma_original` | all three profiles |
| L02 | `NO2 / Asthma.1to18 / AsthmaPrevalence` | `1.10/1.26/1.37; 10/2; ppb x 0.532` | historical_default, c40, krewski_south_africa (3) | blank / recover source | asthma | legacy `FALSE`; no aggregate relation recorded | `asthma_original` | all three profiles |
| L03 | `PM2.5 / Asthma.0to17 / AsthmaERV` | `1.013/1.025/1.037; 10/6; ug x 1` | historical_default, c40, krewski_south_africa (3) | Zheng et al. 2015 / PM2.5 child subgroup identified; retain 6 ug counterfactual and final age semantics as legacy/model decisions | asthma | legacy `FALSE`; no aggregate relation recorded | `asthma_original` | all three profiles |
| L04 | `PM2.5 / Asthma.18to99 / AsthmaERV` | `1.015/1.023/1.031; 10/6; ug x 1` | historical_default, c40, krewski_south_africa (3) | Zheng et al. 2015 / PM2.5 overall value identified; retain 6 ug counterfactual and final age semantics as legacy/model decisions | asthma | legacy `FALSE`; no aggregate relation recorded | `asthma_original` | all three profiles |
| L05 | `O3_8h / Asthma.0to17 / AsthmaERV` | `1.010/1.018/1.024; 10/2; ppb x 0.51` | historical_default, c40, krewski_south_africa (3) | Zheng et al. 2015 / source identified; verify the original age-specific O3 table before migration | asthma | legacy `FALSE`; no aggregate relation recorded | `asthma_original` | all three profiles |
| L06 | `O3_8h / Asthma.18to99 / AsthmaERV` | `1.012/1.018/1.022; 10/2; ppb x 0.51` | historical_default, c40, krewski_south_africa (3) | Zheng et al. 2015 / source identified; verify the original age-specific O3 table before migration | asthma | legacy `FALSE`; no aggregate relation recorded | `asthma_original` | all three profiles |
| L07 | `PM2.5 / PTB / PTB` | `1.07/1.15/1.16; 10/8.8; ug x 1` | historical_default, c40, krewski_south_africa (3) | blank / recover source | birth | legacy `FALSE`; no aggregate relation recorded | `birth_original` | all three profiles |
| L08 | `PM2.5 / LBW / LBW` | `1.03/1.10/1.18; 10/4; ug x 1` | historical_default, c40, krewski_south_africa (3) | blank / recover source | birth | legacy `FALSE`; no aggregate relation recorded | `birth_original` | all three profiles |
| L09 | `PM2.5 / Absences / Absences` | `1.039/1.046/1.053; 10/0; ug x 1` | historical_default, c40, krewski_south_africa (3) | blank / recover source | work absence | legacy `FALSE`; no aggregate relation recorded | `work_absence_original` | all three profiles |
| L10 | `O3_8h / COPD / Deaths` | `1.08/1.12/1.16; 10/35; ppb x 0.51` | historical_default, c40, krewski_south_africa (3) | blank / recover source | mortality | legacy `FALSE`; no aggregate relation recorded | `mortality_original` (provisional) | all three profiles |
| L11 | `O3_8h / COPD / YLLs` | `1.08/1.12/1.16; 10/35; ppb x 0.51` | historical_default, c40, krewski_south_africa (3) | blank / recover source | mortality | legacy `FALSE`; no aggregate relation recorded | `mortality_original` (provisional) | all three profiles |
| L12 | `NO2 / NCD.LRI / Deaths` | `1.021/1.037/1.08; 10/20; ug x 1` | historical_default (1) | blank / recover source | mortality | NCD.LRI aggregate; legacy row `FALSE`; verify target row flag and component-conflict behaviour | `mortality_original` (provisional) | historical_default |
| L13 | `NO2 / NCD.LRI / YLLs` | `1.021/1.037/1.08; 10/20; ug x 1` | historical_default (1) | blank / recover source | mortality | NCD.LRI aggregate; legacy row `FALSE`; verify target row flag and component-conflict behaviour | `mortality_original` (provisional) | historical_default |
| L14 | `SO2 / NCD.LRI / Deaths` | `1.014/1.02/1.026; 5/0.02; ppb x 0.381679389` | historical_default (1) | blank / recover source | mortality | NCD.LRI aggregate; legacy row `FALSE`; verify target row flag and component-conflict behaviour | `mortality_original` (provisional) | historical_default |
| L15 | `SO2 / NCD.LRI / YLLs` | `1.014/1.02/1.026; 5/0.02; ppb x 0.381679389` | historical_default (1) | blank / recover source | mortality | NCD.LRI aggregate; legacy row `FALSE`; verify target row flag and component-conflict behaviour | `mortality_original` (provisional) | historical_default |
| L16 | `NO2 / AllCause / Deaths` | `1.02/1.04/1.06; 10/4.5; ug x 1` | c40, krewski_south_africa (2) | Faustini et al. 2014 / create verified reference row | mortality | reference-specific aggregate; legacy `FALSE`; mentor recalls NO2 AllCause may need overlap treatment—verify | `mortality_original` (provisional) | c40, krewski_south_africa |
| L17 | `NO2 / AllCause / YLLs` | `1.02/1.04/1.06; 10/4.5; ug x 1` | c40, krewski_south_africa (2) | Faustini et al. 2014 / create verified reference row | mortality | reference-specific aggregate; legacy `FALSE`; mentor recalls NO2 AllCause may need overlap treatment—verify | `mortality_original` (provisional) | c40, krewski_south_africa |
| L18 | `NO2 / TotCV / Deaths` | `1.09/1.13/1.18; 10/4.5; ug x 1` | c40, krewski_south_africa (2) | Faustini et al. 2014 / create verified reference row | mortality | reference-specific aggregate; legacy `TRUE`; preserve pending component mapping | `mortality_original` (provisional) | c40, krewski_south_africa |
| L19 | `NO2 / TotCV / YLLs` | `1.09/1.13/1.18; 10/4.5; ug x 1` | c40, krewski_south_africa (2) | Faustini et al. 2014 / create verified reference row | mortality | reference-specific aggregate; legacy `TRUE`; preserve pending component mapping | `mortality_original` (provisional) | c40, krewski_south_africa |
| L20 | `NO2 / TotResp / Deaths` | `1.02/1.03/1.03; 10/4.5; ug x 1` | c40, krewski_south_africa (2) | Faustini et al. 2014 / create verified reference row | mortality | reference-specific aggregate; legacy `TRUE`; preserve pending component mapping | `mortality_original` (provisional) | c40, krewski_south_africa |
| L21 | `NO2 / TotResp / YLLs` | `1.02/1.03/1.03; 10/4.5; ug x 1` | c40, krewski_south_africa (2) | Faustini et al. 2014 / create verified reference row | mortality | reference-specific aggregate; legacy `TRUE`; preserve pending component mapping | `mortality_original` (provisional) | c40, krewski_south_africa |
| L22 | `SO2 / AllCause / Deaths` | `1.014/1.02/1.026; 5/0.02; ppb x 0.381679389` | c40, krewski_south_africa (2) | Krewski et al. 2009 / create verified reference row | mortality | reference-specific aggregate; legacy `FALSE`; verify component coverage before target flag | `mortality_original` (provisional) | c40, krewski_south_africa |
| L23 | `SO2 / AllCause / YLLs` | `1.014/1.02/1.026; 5/0.02; ppb x 0.381679389` | c40, krewski_south_africa (2) | Krewski et al. 2009 / create verified reference row | mortality | reference-specific aggregate; legacy `FALSE`; verify component coverage before target flag | `mortality_original` (provisional) | c40, krewski_south_africa |
| L24 | `PM2.5 / AllCause / Deaths` | `1.027/1.043/1.06; 10/5.8; ug x 1` | krewski_south_africa (1) | Krewski et al. 2009 / create verified reference row | mortality | reference-specific aggregate; legacy `FALSE`; verify component coverage before target flag | `mortality_original` (provisional) | krewski_south_africa |
| L25 | `PM2.5 / AllCause / YLLs` | `1.027/1.043/1.06; 10/5.8; ug x 1` | krewski_south_africa (1) | Krewski et al. 2009 / create verified reference row | mortality | reference-specific aggregate; legacy `FALSE`; verify component coverage before target flag | `mortality_original` (provisional) | krewski_south_africa |

## Non-active historical artifact: C40 PM2.5 variant

`inst/extdata/rr/processed/CRFs_C40_PM25.csv` is not selected by the active
`crfs_version` routing in `R/hia_data.R`; it is therefore not part of the
55-row active migration scope above. Preserve it for audit, but do not silently
merge it into C40.

It contains an additional `PM2.5 / AllCause / Deaths` row attributed in raw text
to Chen and Hoek (2020), with RR `1.06/1.08/1.09`, delta/counterfactual `10/10`,
and a malformed unquoted comma in the reference field. It is both an orphaned
workflow artifact and invalid CSV for ordinary parsing. Confirm its intended
project use and repair/recover its provenance in a separate decision before
including it in a Set or registry.

## Required verification before implementation

1. Produce a test inventory that reproduces the 15/19/21 profile counts from
   the manifest's selected registry rows.
2. Resolve every `recover source` entry to a full reference record and stable
   `reference_id`.
3. Verify age bounds for the default-only rows and every default-table row that
   omitted them.
4. Verify aggregate/component membership and target `double_counted` behaviour
   from the reference for L12-L25.
5. Test that user-facing discovery cannot return fixtures, while deterministic
   tests can still use an internal fixture source.
6. Compare each migrated CRF's PAF numerically with its relevant legacy path,
   with non-`NA` and non-zero guards where an impact calculation is exercised.

## Related readiness audit

`10-default-profile-migration-readiness-audit.md` records the row-by-row
reference, age, double-counting, and Registry status for the 15-row default
profile. It is the implementation gate for the first historical fingerprint
target; it does not declare the profile migrated or select a public preset.

`11-scientific-review-open-questions.md` is the separate evidence-backed review
queue for questions that cannot be answered by source recovery or code tracing
alone. Q01 centralises the asthma ERV age-field decision affecting L03–L06.
