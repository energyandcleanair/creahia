# Default Historical CRF Profile: Migration Readiness Audit

## Scope and status

This audit covers exactly the 15 rows in the active legacy
`CRFs.csv`/`legacy_profile_default` profile. It records local code and data
evidence only; it does not infer missing scientific provenance.

No row is `ready_to_migrate` as of this audit. A row is ready only when its
reference, age applicability, double-counting behaviour, and Registry metadata
are all supported by evidence.

## How to read the statuses

| Status | Meaning |
| --- | --- |
| `needs_reference_recovery` | The legacy file has no usable reference and `references.csv` has no stable source record. |
| `reference_recovered_with_derivation_pending` | The original study is recorded, but a legacy unit conversion or model setting is not stated directly by that study and must remain documented as such. |
| `age_corroborated` | The identical C40 and South-Africa row provides matching numeric age bounds. |
| `age_boundary_check` | A counterpart provides bounds, but its named age group and the legacy cause label differ at a boundary. |
| `needs_age_evidence` | The default-only row has no counterpart with age bounds. |
| `legacy_false_pending_reference` | Legacy metadata says `double_counted = FALSE`; retain it as source evidence but do not treat it as a scientific conclusion until the reference is recovered. |
| `needs_aggregate_decision` | The row uses `NCD.LRI`; aggregate/component overlap must be verified from the source before final Registry metadata. |
| `partially_migrated` | A Registry row exists but still has unresolved provenance or metadata. |
| `not_migrated` | No Registry row currently represents this definition. |

## Evidence summary

| Audit area | Result | Consequence |
| --- | --- | --- |
| Reference | All 15 default rows have blank legacy reference fields. L01 now has a recovered source record; L12 still uses the temporary `legacy_default_crfs` record. | L02–L15 still need a recovered source and stable `reference_id`; L02 also has an outcome-definition concern. |
| Age | L01–L11 have C40/South-Africa counterparts with numeric bounds. L03 and L05 need an age-boundary convention check. L12–L15 have no counterpart. | L12–L15 cannot receive evidence-backed Registry ages yet. |
| Double counting | L01–L11 are legacy `FALSE` and are not aggregate labels. L12–L15 are legacy `FALSE` but use aggregate `NCD.LRI`. | Preserve source flags provisionally; investigate L12–L15 before finalisation. |
| Registry | L01 and L12 exist as Registry rows. The other 13 definitions are absent. | Existing rows must be corrected or evidenced before they can be selected in a historical target. |

## Row-by-row audit

| ID | Legacy CRF | Reference readiness | Age readiness | Double-counting readiness | Registry migration status | Readiness and next required evidence |
| --- | --- | --- | --- | --- | --- | --- |
| L01 | `NO2 / Asthma.1to18 / AsthmaIncidence` | `reference_recovered_with_derivation_pending`: Khreis et al. (2017), `khreis_2017_childhood_asthma`; the paper reports OR 1.05 (1.02–1.07) per 4 ug/m3, while the legacy 10 ppb RR is a documented rescaling | `age_corroborated`: 1–18 in C40 and South Africa; the paper itself does not establish the Registry boundary | `legacy_false_pending_coverage` | `migrated_with_follow_up`: `legacy_no2_asthma_1to18_incidence_v1` now has a stable reference ID | Retain the conversion, 2 ppb counterfactual, and age range as legacy/model settings; add this row to a historical selection only after the 15-row coverage test. |
| L02 | `NO2 / Asthma.1to18 / AsthmaPrevalence` | `needs_scientific_source_decision`: the recovered Khreis CRF is for asthma development/incidence, not prevalence; it must not be reused automatically | `age_corroborated`: 1–18 | `legacy_false_pending_reference` | `not_migrated` | Identify the original prevalence source or explicitly decide whether this legacy outcome should be retired. |
| L03 | `PM2.5 / Asthma.0to17 / AsthmaERV` | `needs_reference_recovery`; legacy field blank | `age_boundary_check`: cause says `0to17`; counterparts store age 0–18 | `legacy_false_pending_reference` | `not_migrated` | Recover reference and determine whether age high `18` is an inclusive bound or a label convention. |
| L04 | `PM2.5 / Asthma.18to99 / AsthmaERV` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 18–99 | `legacy_false_pending_reference` | `not_migrated` | Recover reference, then add Registry row with 18–99 bounds. |
| L05 | `O3_8h / Asthma.0to17 / AsthmaERV` | `needs_reference_recovery`; legacy field blank | `age_boundary_check`: cause says `0to17`; counterparts store age 0–18 | `legacy_false_pending_reference` | `not_migrated` | Recover reference and resolve the same 0–17/0–18 boundary convention as L03. |
| L06 | `O3_8h / Asthma.18to99 / AsthmaERV` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 18–99 | `legacy_false_pending_reference` | `not_migrated` | Recover reference, then add Registry row with 18–99 bounds. |
| L07 | `PM2.5 / PTB / PTB` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 0–0 (`Newborn`) | `legacy_false_pending_reference` | `not_migrated` | Recover reference and confirm that the Registry age encoding represents the newborn rule correctly. |
| L08 | `PM2.5 / LBW / LBW` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 0–0 (`Newborn`) | `legacy_false_pending_reference` | `not_migrated` | Recover reference and confirm that the Registry age encoding represents the newborn rule correctly. |
| L09 | `PM2.5 / Absences / Absences` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 20–65 | `legacy_false_pending_reference` | `not_migrated` | Recover reference, then add Registry row with 20–65 bounds. |
| L10 | `O3_8h / COPD / Deaths` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 25–99 | `legacy_false_pending_reference` | `not_migrated` | Recover reference, then add Registry row with 25–99 bounds. |
| L11 | `O3_8h / COPD / YLLs` | `needs_reference_recovery`; legacy field blank | `age_corroborated`: 25–99 | `legacy_false_pending_reference` | `not_migrated` | Recover reference, then add Registry row with 25–99 bounds. |
| L12 | `NO2 / NCD.LRI / Deaths` | `needs_reference_recovery`; legacy field blank; current row uses temporary `legacy_default_crfs` | `needs_age_evidence`: default-only; current Registry 25–Inf is not corroborated by legacy profile data | `needs_aggregate_decision`: legacy `FALSE` is not enough to establish NCD.LRI overlap handling | `partially_migrated`: `legacy_no2_ncdlri_deaths_v1` | Recover reference, establish age applicability, and verify aggregate/component handling before retaining or replacing this row. |
| L13 | `NO2 / NCD.LRI / YLLs` | `needs_reference_recovery`; legacy field blank | `needs_age_evidence`: default-only | `needs_aggregate_decision` | `not_migrated` | Recover reference, establish age applicability, and verify aggregate/component handling. |
| L14 | `SO2 / NCD.LRI / Deaths` | `needs_reference_recovery`; legacy field blank | `needs_age_evidence`: default-only | `needs_aggregate_decision` | `not_migrated` | Recover reference, establish age applicability, and verify aggregate/component handling. |
| L15 | `SO2 / NCD.LRI / YLLs` | `needs_reference_recovery`; legacy field blank | `needs_age_evidence`: default-only | `needs_aggregate_decision` | `not_migrated` | Recover reference, establish age applicability, and verify aggregate/component handling. |

## What can and cannot be done next

### Can proceed after source recovery

L01–L11 have a numeric age counterpart in the C40/South-Africa files. Once a
reference is recovered, each can be migrated in a reviewable data change. L03
and L05 additionally need a shared decision on how Registry age bounds encode
the legacy `Asthma.0to17` naming convention.

### Requires a scientific decision before final Registry metadata

L12–L15 must not be finalised from the default file alone. Their `NCD.LRI`
label is an aggregate, their legacy rows omit age bounds and provenance, and
their `double_counted = FALSE` flag does not establish the component-overlap
rule. The existing L12 Registry row is a validation/migration placeholder, not
evidence that those questions are resolved.

## Source recovery completed in this slice

L01 is the first source-record recovery slice. The Registry now records Khreis
et al. (2017) as `khreis_2017_childhood_asthma`, rather than the temporary
`legacy_default_crfs` placeholder. The source reports an OR of 1.05
(1.02–1.07) per 4 ug/m3 NO2. The legacy RR 1.26 (1.10–1.37) per 10 ppb is a
documented rescaling using 1 ppb = 1.88 ug/m3 at 25C; the 2 ppb counterfactual
and the 1–18 Registry range are legacy/model settings, not values stated in
that paper.

This evidence cannot be transferred to L02: L02 is labelled
`AsthmaPrevalence`, whereas the Khreis CRF describes asthma development. That
distinction is retained explicitly so a later migration cannot silently treat
incidence and prevalence as interchangeable.

## Exit criteria for `legacy_profile_default`

The default profile can populate a historical `target_selection_id` only when:

1. all 15 definitions have stable `reference_id` values linked to recovered
   references;
2. every row has evidence-backed age bounds, including L12–L15;
3. final `double_counted` metadata and any relevant `dc_groups.csv` treatment
   are verified for the NCD.LRI rows;
4. the Registry contains exactly one matching row per definition, with the
   same RR, counterfactual, unit, and multiplier as `CRFs.csv`; and
5. a coverage test proves that the selected Registry rows reproduce the 15-row
   legacy profile before fingerprint comparison begins.

## Evidence sources inspected

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `inst/extdata/rr/processed/CRFs_Krewski_SouthAfrica.csv`
- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/references.csv`
- `08-legacy-crf-migration-manifest.md`
- Khreis H, Kelly C, Tate J, et al. (2017), doi:10.1016/j.envint.2016.11.012
