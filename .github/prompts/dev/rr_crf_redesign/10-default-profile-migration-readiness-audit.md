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
| `source_identified_pending_parameter_verification` | The originating study is recorded, but an age-specific value, counterfactual, or age-routing convention remains to be verified before Registry migration. |
| `age_corroborated` | The identical C40 and South-Africa row provides matching numeric age bounds. |
| `age_boundary_check` | A counterpart provides bounds, but its named age group and the legacy cause label differ at a boundary. |
| `needs_age_evidence` | The default-only row has no counterpart with age bounds. |
| `needs_scientific_review: Q01` | Code and source evidence are recorded, but the Registry age-field contract still needs scientific/semantic review. |
| `needs_scientific_review: Q02` | Legacy parameter provenance is traced, but no source supports the outcome label; reviewers must decide whether to retire or replace it. |
| `needs_scientific_review: Q03` | Source parameters are recovered, but the study effect measure or outcome population cannot yet be represented safely by the current Registry contract. |
| `needs_scientific_review: Q04` | Source age or exposure semantics are recovered, but the current output/forecast path does not preserve them. |
| `needs_scientific_review: Q05` | The historical row changes the source outcome, age, counterfactual, or exposure-metric semantics; reviewers must approve the target definition. |
| `needs_scientific_review: Q06` | The original NO2 mortality source is recovered, but the legacy outcome, age metadata, and PM2.5-overlap adjustment are a separate implementation whose target contract needs review. |
| `needs_scientific_review: Q07` | The original SO2 mortality source is recovered, but the legacy outcome, age metadata, YLL application, and concentration conversion are separate implementation choices whose target contract needs review. |
| `needs_scientific_review: Q08` | The effect-estimate source and HIA implementation source are both recovered, but the single-reference Registry schema cannot represent their distinct roles. |
| `legacy_false_pending_reference` | Legacy metadata says `double_counted = FALSE`; retain it as source evidence but do not treat it as a scientific conclusion until the reference is recovered. |
| `needs_aggregate_decision` | The row uses `NCD.LRI`; aggregate/component overlap must be verified from the source before final Registry metadata. |
| `partially_migrated` | A Registry row exists but still has unresolved provenance or metadata. |
| `not_migrated` | No Registry row currently represents this definition. |

## Evidence summary

| Audit area | Result | Consequence |
| --- | --- | --- |
| Reference | All 15 default rows have blank legacy reference fields. L01 and L03–L15 now have recovered source records; L02's identical RR is traced to an incidence-only source. L12's existing Registry row still uses temporary `legacy_default_crfs` metadata. | L01 needs Q08 for dual-source provenance representation; L12–L13 need Q06 and L14–L15 need Q07 before their sources can be represented safely; L02 needs Q02, L07–L08 need Q03, L09 needs Q04, and L10–L11 need Q05 before final Registry metadata. |
| Age | L01–L11 have C40/South-Africa counterparts with numeric bounds. L03–L06 need Q01: L03/L05 use child estimates, whereas adult-labelled L04/L06 use Zheng overall estimates; the C40 `18-Jan` display value is not authoritative. L12–L15 have no counterpart. | L03–L06 need a joint effect-estimate/age-routing decision; L12–L15 cannot receive evidence-backed Registry ages yet. |
| Double counting | L01–L11 are legacy `FALSE` and are not aggregate labels. L12–L15 are legacy `FALSE` but use aggregate `NCD.LRI`. | Preserve source flags provisionally; investigate L12–L15 before finalisation. |
| Registry | L01 and L12 exist as Registry rows. The other 13 definitions are absent. | Existing rows must be corrected or evidenced before they can be selected in a historical target. |

## Row-by-row audit

| ID | Legacy CRF | Reference readiness | Age readiness | Double-counting readiness | Registry migration status | Readiness and next required evidence |
| --- | --- | --- | --- | --- | --- | --- |
| L01 | `NO2 / Asthma.1to18 / AsthmaIncidence` | `needs_scientific_review: Q08`: Khreis et al. (2017) is the underlying association source, while Achakulwisut et al. (2019) documents the HIA 1–18 and 2 ppb implementation settings; current Registry row can reference only Khreis | `age_corroborated`: 1–18 in C40 and South Africa; the paper itself does not establish the Registry boundary | `legacy_false_pending_coverage` | `migrated_with_follow_up`: `legacy_no2_asthma_1to18_incidence_v1` has a primary reference but incomplete structured provenance | Resolve Q08 before presenting the Registry row as source-complete; retain the conversion, 2 ppb counterfactual, and age range as documented historical HIA settings. |
| L02 | `NO2 / Asthma.1to18 / AsthmaPrevalence` | `needs_scientific_review: Q02`: the identical legacy RR is traced through Khreis et al. (2017) and Achakulwisut et al. (2019), both of which support incidence rather than prevalence | `age_corroborated`: 1–18 | `legacy_false_pending_reference` | `not_migrated` | Resolve Q02: retire the unsupported historical prevalence row, or approve a separate evidence-backed prevalence CRF as an explicit scientific change. |
| L03 | `PM2.5 / Asthma.0to17 / AsthmaERV` | `source_identified_pending_parameter_verification`: Zheng et al. (2015), `zheng_2015_asthma_erv`; child PM2.5 RR 1.025 (1.013–1.037) per 10 ug/m3 is corroborated, while the 6 ug counterfactual is a legacy/model setting | `needs_scientific_review: Q01`: legacy cause `0to17`, counterpart metadata 0–18, and legacy output label 0–18 have distinct roles | `legacy_false_pending_reference` | `not_migrated` | Verify the O3-independent EPI routing evidence, then resolve Q01 before assigning final Registry bounds. |
| L04 | `PM2.5 / Asthma.18to99 / AsthmaERV` | `needs_scientific_review: Q01`: Zheng et al. (2015), `zheng_2015_asthma_erv`; the legacy value matches the PM2.5 **overall** RR 1.023 (1.015–1.031) per 10 ug/m3, not an adult subgroup estimate; 6 ug counterfactual is legacy/model setting | `needs_scientific_review: Q01`: counterpart metadata is 18–99 but legacy impact output label is 25+ | `legacy_false_pending_reference` | `not_migrated` | Resolve Q01: decide whether an overall-age effect estimate may remain on an adult-labelled EPI route before assigning Registry bounds. |
| L05 | `O3_8h / Asthma.0to17 / AsthmaERV` | `needs_reference_recovery`: Zheng et al. (2015) gives the matching O3 overall central estimate but does not yet verify legacy `1.018 (1.010–1.024)` as a child subgroup value after unit conversion | `needs_scientific_review: Q01`: recover the O3 parameter source before reviewing child age semantics | `legacy_false_pending_reference` | `not_migrated` | Inspect Zheng Table 2 and supporting data for the exact O3 estimate; if absent, recover the actual L05 source before assigning Registry bounds. |
| L06 | `O3_8h / Asthma.18to99 / AsthmaERV` | `needs_scientific_review: Q01`: Zheng et al. (2015), `zheng_2015_asthma_erv`; the legacy value matches the O3 **overall** estimate after the legacy unit conversion, not an adult subgroup estimate | `needs_scientific_review: Q01`: counterpart metadata is 18–99 but legacy impact output label is 25+ | `legacy_false_pending_reference` | `not_migrated` | Resolve Q01: decide whether an overall-age effect estimate may remain on an adult-labelled EPI route before assigning Registry bounds. |
| L07 | `PM2.5 / PTB / PTB` | `needs_scientific_review: Q03`: Trasande et al. (2016), `trasande_2016_preterm_birth`, reports base-case OR 1.15 per 10 ug/m3 and 8.8 ug/m3 reference; legacy 1.07/1.16 are the paper's low/high scenarios, not CI bounds | `age_corroborated`: 0–0 (`Newborn`) | `legacy_false_pending_reference` | `not_migrated` | Resolve Q03 before final Registry metadata: determine whether the reported OR can be used in the historical log-linear RR formula and how scenario values are represented. |
| L08 | `PM2.5 / LBW / LBW` | `needs_scientific_review: Q03`: Dadvand et al. (2013), `dadvand_2013_term_lbw`, matches 1.10 (1.03–1.18) per 10 ug/m3 for term LBW; the 4 ug/m3 counterfactual is a legacy/model setting and the paper reports an OR | `age_corroborated`: 0–0 (`Newborn`) | `legacy_false_pending_reference` | `not_migrated` | Resolve Q03 and confirm the legacy LBW EPI metric is term LBW before final Registry metadata. |
| L09 | `PM2.5 / Absences / Absences` | `needs_scientific_review: Q04`: WHO HRAPIE (2013), `who_hrapie_2013`, matches short-term Group B* RR 1.046 (1.039–1.053) for work days lost at ages 20–65; source specifies two-week PM2.5 converted to annual average and requires subtracting work days lost from RADs when burdens are combined | `needs_scientific_review: Q04`: CRF metadata is 20–65, but legacy final output uses fallback label 25+ | `legacy_false_pending_reference` | `not_migrated` | Resolve Q04: preserve source population and exposure-duration semantics, and document/implement the required RAD subtraction rather than treating `FALSE` as sufficient. |
| L10 | `O3_8h / COPD / Deaths` | `needs_scientific_review: Q05`: Turner et al. (2016), `turner_2016_long_term_ozone_mortality`, is a PM2.5/NO2-adjusted respiratory-mortality HR source; Malley et al. (2017), `malley_2017_ozone_respiratory_mortality`, is the global respiratory-mortality implementation and contextualises COPD at about 70–71% of estimated respiratory mortality | `needs_scientific_review: Q05`: source/model population is 30+, while legacy metadata is 25–99 | `legacy_false_pending_reference` | `not_migrated` | Resolve Q05 before final Registry metadata: approve or replace the COPD mapping, 25–99 bounds, 35 ppb counterfactual, annual-MDA8 exposure interpretation, and overlap rule. |
| L11 | `O3_8h / COPD / YLLs` | `needs_scientific_review: Q05`: same recovered Turner/Malley evidence as L10 | `needs_scientific_review: Q05`: source/model population is 30+, while legacy metadata is 25–99 | `legacy_false_pending_reference` | `not_migrated` | Resolve Q05 before final Registry metadata, including whether a respiratory-mortality HR may apply to COPD YLLs. |
| L12 | `NO2 / NCD.LRI / Deaths` | `needs_scientific_review: Q06`: WHO HRAPIE (2013), `who_hrapie_2013`, recommends all-natural-cause mortality RR 1.055 (1.031–1.080), not the legacy NCD.LRI outcome or its adjusted central/lower RRs; legacy 20 ug/m3 counterfactual faithfully retains the source threshold | `needs_scientific_review: Q06`: source population is 30+; default-only row omits age and existing Registry 25–Inf is not evidence-backed | `needs_scientific_review: Q06`: HRAPIE requires overlap-aware Group B* treatment with PM2.5; legacy `FALSE` is not a valid target rule | `partially_migrated`: `legacy_no2_ncdlri_deaths_v1` still uses temporary provenance and unreviewed 25–Inf metadata | Resolve Q06 before retaining, replacing, or selecting this row: approve the historic outcome/overlap adjustment or create a source-faithful target definition. |
| L13 | `NO2 / NCD.LRI / YLLs` | `needs_scientific_review: Q06`: same recovered HRAPIE evidence as L12; legacy 20 ug/m3 counterfactual retains the source threshold, but HRAPIE is an all-natural-cause mortality CRF, not a direct NCD.LRI-YLL CRF | `needs_scientific_review: Q06`: source population is 30+; default-only row omits age | `needs_scientific_review: Q06`: Group B* overlap handling is required; legacy `FALSE` is not a valid target rule | `not_migrated` | Resolve Q06, including whether the source mortality CRF can apply to YLLs, before final Registry metadata. |
| L14 | `SO2 / NCD.LRI / Deaths` | `needs_scientific_review: Q07`: Krewski et al. (2009), `krewski_2009_so2_all_cause_mortality`, matches all-cause mortality HR 1.020 (1.014–1.026) per 5 ppb for SO2 (1980), not the legacy NCD.LRI outcome | `needs_scientific_review: Q07`: source cohort is 30+; default-only row omits age, while counterpart profiles use 25–99 | `needs_scientific_review: Q07`: no source evidence establishes the NCD.LRI aggregate relationship or legacy `FALSE` target rule | `not_migrated` | Resolve Q07 before Registry migration: approve a source-faithful all-cause definition or retain the historical NCD.LRI mapping explicitly, including the 0.02 ppb counterfactual and ug-to-ppb conversion. |
| L15 | `SO2 / NCD.LRI / YLLs` | `needs_scientific_review: Q07`: same recovered Krewski all-cause mortality evidence as L14; it is not a direct NCD.LRI-YLL CRF | `needs_scientific_review: Q07`: source cohort is 30+; default-only row omits age | `needs_scientific_review: Q07`: no source evidence establishes the NCD.LRI aggregate relationship or legacy `FALSE` target rule | `not_migrated` | Resolve Q07, including whether the all-cause mortality CRF can apply to YLLs, before final Registry metadata. |

## What can and cannot be done next

### Can proceed after source recovery

L01 and L03–L11 have a numeric age counterpart in the C40/South-Africa files.
Once their source and other row-specific evidence are recovered, each can be
migrated in a reviewable data change. L03 and L05 additionally need a shared
decision on how Registry age bounds encode the legacy `Asthma.0to17` naming
convention. L02 is deliberately excluded: it is governed by Q02 rather than
ordinary reference recovery; L07–L08 are governed by Q03 after their source
recovery, L09 is governed by Q04, and L10–L11 are governed by Q05.

### Requires a scientific decision before final Registry metadata

L12–L15 must not be finalised from the default file alone. Their `NCD.LRI`
label is an aggregate, their legacy rows omit age bounds and provenance, and
their `double_counted = FALSE` flag does not establish the component-overlap
rule. L12–L13 now have a recovered HRAPIE source, but it is for all-natural-cause
mortality at ages 30+ and requires PM2.5-overlap-aware treatment; Q06 therefore
remains an implementation and scientific gate. L14–L15 now have a recovered
Krewski source, but it is for all-cause mortality in a 30+ cohort; Q07 is the
corresponding outcome, age, YLL, and conversion gate. The existing L12 Registry
row is a validation/migration placeholder, not evidence that those questions are
resolved.

## Source recovery completed in this slice

L01 is the first source-record recovery slice. The Registry now records Khreis
et al. (2017) as `khreis_2017_childhood_asthma`, rather than the temporary
`legacy_default_crfs` placeholder. The source reports an OR of 1.05
(1.02–1.07) per 4 ug/m3 NO2. The legacy RR 1.26 (1.10–1.37) per 10 ppb is a
documented rescaling using 1 ppb = 1.88 ug/m3 at 25C. Khreis does not state
the 2 ppb counterfactual or a 1–18 boundary; Achakulwisut et al. (2019)
documents both as historical HIA application settings.

This evidence cannot be transferred to L02: L02 is labelled
`AsthmaPrevalence`, whereas the Khreis CRF describes asthma development. That
distinction is retained explicitly so a later migration cannot silently treat
incidence and prevalence as interchangeable.

Further recovery confirms that the duplicate legacy RR is not an independent
prevalence CRF: Achakulwisut et al. (2019) applies the same 1.26
(1.10–1.37) per 10 ppb association to new paediatric asthma incidence at ages
1–18. Q02 in the scientific-review queue therefore asks whether the legacy
prevalence row should be retired or replaced by a separately approved source.

## Zheng asthma ERV source-identification slice

L03–L06 now point to Zheng et al. (2015) through
`zheng_2015_asthma_erv`; this identifies the epidemiological source without
claiming that all Registry metadata is final. L03 matches a child subgroup
estimate; L05's exact O3 parameter source remains unverified; and
adult-labelled L04/L06 match overall estimates. Before
adding any of the four Registry rows, resolve the discrepancy between legacy
cause labels (`0to17`, `18to99`), counterpart metadata (`0–18`, `18–99`),
the overall-versus-subgroup effect estimates, and current legacy impact labels
(`0–18`, `25+`).

## Scientific-review link

`11-scientific-review-open-questions.md` records Q01, the shared question of
what Registry `age_low` / `age_high` should mean for the asthma ERV rows. The
question is intentionally centralised there: this audit tracks row readiness,
while Q01 separates confirmed evidence, remaining engineering tracing, and the
later scientific/semantic decision. It does not block reference recovery for
the other legacy rows.

Q02 records the separate outcome-definition decision for L02. It prevents the
identical incidence RR from being presented as a recovered prevalence CRF.

Q03 records the birth-outcome effect-measure decision shared by L07 and L08.
It preserves their recovered source evidence without treating reported ORs as
unqualified Registry RRs or assuming that L08's EPI outcome is term LBW.

Q04 records the work-absence population and exposure-duration semantics for
L09. It prevents the source's 20–65 working-age population from being silently
changed to the legacy output/forecast fallback of 25+.

Q05 records the long-term O3 respiratory-mortality evidence for L10–L11. It
prevents a respiratory HR, source age 30+, and source/model cutoff from being
silently relabelled as COPD, 25–99, and 35 ppb Registry metadata.

Q06 records the long-term NO2 all-natural-cause mortality source for L12–L13.
It prevents the source outcome, age 30+, and PM2.5-overlap treatment from being
silently relabelled as NCD.LRI, 25+, and `double_counted = FALSE` metadata.

Q07 records the long-term SO2 all-cause mortality source for L14–L15. It
prevents the source outcome and age 30+ from being silently relabelled as
NCD.LRI and 25+, and records the unresolved YLL and concentration-conversion
semantics.

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
- Achakulwisut P, Brauer M, Hystad P, Anenberg SC. (2019), doi:10.1016/S2542-5196(19)30046-4
- Zheng X-Y, Ding H, Jiang L-N, et al. (2015), doi:10.1371/journal.pone.0138146
- Trasande L, Malecha P, Attina TM. (2016), doi:10.1289/ehp.1510810
- Dadvand P, Parker J, Bell ML, et al. (2013), doi:10.1289/ehp.1205575
- WHO Regional Office for Europe (2013), HRAPIE project report
- Turner MC, Jerrett M, Pope CA III, et al. (2016), doi:10.1164/rccm.201508-1633OC
- Malley CS, Henze DK, Kuylenstierna JCI, et al. (2017), doi:10.1289/EHP1390
