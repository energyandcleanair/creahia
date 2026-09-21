# CREAHIA Registry Legacy Default Migration and Scientific Review

This is the reviewer-facing source document for the 15 active rows in `legacy_profile_default`. It combines the former migration-readiness audit, scientific questions, and review pack.

## Purpose

CREAHIA is moving from separate legacy concentration-response function (CRF) tables to a queryable and testable Registry. This is an intentional breaking change: the new package should not preserve old parameter-conversion logic. When a Registry selection represents an old calculation, numerical non-regression tests must show that its PAF/HIA outputs match the legacy path.

Most literature and code evidence for the default profile has now been recovered. Some questions cannot be decided from R code or an original paper alone, for example whether a source outcome supports the historical outcome, what an age range means, or how pollutant overlap should be handled. This document asks HIA-domain reviewers and the package mentor to choose a direction or propose an alternative.

## How to review this document

1. Read **Confirmed evidence** first. These statements are traced to source papers, legacy data, or code.
2. Read **Decision requested** next. These are scientific or schema decisions; reviewers do not need to review R code.
3. Choose an option, amend one, or propose another approach.
4. If a decision intentionally changes historical results, the implementation will record an authorised fingerprint difference rather than silently change output.

## Scope and completed safeguards

| Scope | Current status | Why it matters |
| --- | --- | --- |
| Legacy default profile | All 15 active CRF rows have been reviewed individually. | A legacy row is not automatically suitable for migration. |
| Numerical safety net | A fixed 15-row legacy PAF snapshot has been created. | Future Registry CRF/PAF results can be compared row by row. |
| Open decisions | Q01–Q08 below. | Only scientific or schema decisions that remain after evidence recovery are included. |

## 15-row migration readiness summary

No row in `legacy_profile_default` is currently `ready_to_migrate`. This is a reviewer-friendly summary. The complete technical migration inventory, with 25 CRF definitions and 55 active source-row memberships, remains in [the legacy CRF migration manifest](../.github/prompts/dev/rr_crf_redesign/08-legacy-crf-migration-manifest.md).

| Legacy row | CRF | Core decision needed from reviewers |
| --- | --- | --- |
| L01 | NO2 / Asthma incidence | Q08: Khreis is the association source; Achakulwisut documents the 1–18 population and 2 ppb HIA settings; the schema cannot represent both roles. |
| L02 | NO2 / Asthma prevalence | Q02: matching RR sources support incidence, not prevalence; retire the row or select a separate source. |
| L03 | PM2.5 / child Asthma ERV | Q01: child RR is confirmed; define the relationship between Registry age fields and the EPI route. |
| L04 | PM2.5 / adult-labelled Asthma ERV | Q01: the adult-labelled legacy route uses an overall RR, not an adult subgroup RR. |
| L05 | O3 / child Asthma ERV | Q01: overall central RR and child CI are mixed; retain only for historical comparison or replace with one complete estimate. |
| L06 | O3 / adult-labelled Asthma ERV | Q01: the adult-labelled legacy route uses an overall RR, not an adult subgroup RR. |
| L07 | PM2.5 / PTB | Q03: Trasande reports an OR; 1.07–1.16 are scenarios, not a CI. |
| L08 | PM2.5 / LBW | Q03: the source reports an OR for term LBW; confirm whether the legacy EPI metric is term LBW. |
| L09 | PM2.5 / Absences | Q04: HRAPIE specifies ages 20–65 and short-term exposure; legacy EPI, output, and forecast age paths differ. |
| L10 | O3 / COPD deaths | Q05: the source is respiratory mortality in adults 30+, not a COPD-specific RR. |
| L11 | O3 / COPD YLLs | Q05: beyond L10, approve or reject use for COPD YLLs. |
| L12 | NO2 / NCD.LRI deaths | Q06: HRAPIE is all-natural-cause mortality at 30+; legacy NCD.LRI, YLL, and PM2.5-overlap treatment need review. |
| L13 | NO2 / NCD.LRI YLLs | Q06: as for L12; the source is not a direct NCD.LRI-YLL CRF. |
| L14 | SO2 / NCD.LRI deaths | Q07: Krewski is all-cause mortality at 30+, not NCD.LRI. |
| L15 | SO2 / NCD.LRI YLLs | Q07: as for L14; the source does not support this YLL application. |

### Overall readiness conclusions

- L01 and L03–L11 have numerical age counterparts in the C40/South Africa legacy tables, but that does not replace source or scientific verification.
- L12–L15 use the `NCD.LRI` aggregate. Legacy `double_counted = FALSE` is not itself a scientific conclusion that can be copied into Registry metadata.
- `age_group = 18-Jan` in `CRFs_C40.csv` is an Excel display corruption. It does not currently affect legacy calculation or EPI routing, but must be fixed before that file is treated as authoritative metadata.

## Review-question overview

| Question | Topic | One-sentence summary |
| --- | --- | --- |
| Q01 | Asthma ERV | Are age metadata and effect estimates scientifically consistent? |
| Q02 | NO2 asthma prevalence | Should a legacy row without prevalence evidence be retired? |
| Q03 | Birth outcomes | How should ORs and scenario ranges be represented honestly? |
| Q04 | Work absences | How should HRAPIE's 20–65 population be preserved through outputs and forecasts? |
| Q05 | Long-term O3 mortality | Can respiratory mortality be represented as COPD deaths/YLLs? |
| Q06 | Long-term NO2 mortality | Can all-natural-cause mortality be represented as NCD.LRI deaths/YLLs? |
| Q07 | Long-term SO2 mortality | Can all-cause mortality be represented as NCD.LRI deaths/YLLs? |
| Q08 | Provenance schema | How should one CRF record two distinct reference roles? |

## Q01 What should Registry age fields mean for asthma ERV CRFs?

**Affected legacy rows:** L03–L06.

### Legacy rows covered by this question

- **L03 — PM2.5 / Asthma.0to17 / AsthmaERV:** child-labelled; RR matches Zheng's PM2.5 child subgroup.
- **L04 — PM2.5 / Asthma.18to99 / AsthmaERV:** adult-labelled; RR matches Zheng's PM2.5 overall estimate.
- **L05 — O3_8h / Asthma.0to17 / AsthmaERV:** child-labelled; the legacy central RR and CI come from different Zheng estimates.
- **L06 — O3_8h / Asthma.18to99 / AsthmaERV:** adult-labelled; RR matches Zheng's O3 overall estimate.

### Confirmed evidence

- Zheng et al. 2015 supports the RR provenance, but uses broad groups such as children, adults, and elderly. It does not define CREAHIA's intervals such as 0–17, 0–18, 18–99, or 25+.
- L03 matches Zheng's PM2.5 child subgroup; L04 and L06 are adult-labelled in legacy routing but match overall, rather than adult subgroup, estimates.
- L05 mixes the central RR from Zheng's overall O3 estimate with the CI from the child subgroup. It is not a complete estimate directly reported in the paper.
- Legacy EPI routing uses a `cause`/`outcome` key, whereas final age labels use another rule. One pair of `age_low`/`age_high` fields cannot safely represent study population, EPI route, and output label at once.

### Decision requested

What should Registry age fields represent? May L04/L06 retain overall effect estimates on adult EPI routes? Should L05 be historical-only or replaced by a complete Zheng estimate?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Retain historical rows with explicit limits | Keep L04–L06 only in a historical Set; do not treat L05 as a complete scientific CRF. Historical numerical comparison remains possible. |
| B. Use internally consistent source estimates | Replace L05 with a complete child O3 estimate and choose age-appropriate estimates for L04/L06. Record numerical changes as authorised differences. |
| C. Expand age semantics in the Registry | Store study population, EPI route, and output label separately instead of overloading one age interval. |

**Reference:** [Zheng et al. 2015](https://doi.org/10.1371/journal.pone.0138146)

## Q02 Should the legacy NO2 asthma-prevalence row be retired?

**Affected legacy row:** L02, `NO2 / Asthma.1to18 / AsthmaPrevalence`.

### Legacy row covered by this question

- **L02 — NO2 / Asthma.1to18 / AsthmaPrevalence:** uses the same RR as the incidence row, but recovered sources support asthma incidence/development, not prevalence.

### Confirmed evidence

- L01 incidence and L02 prevalence have identical legacy RR, counterfactual, and age parameters.
- Khreis et al. 2017 is an association with childhood asthma development; Achakulwisut et al. 2019 applies the RR to new paediatric asthma incidence.
- No original study or method source was found that supports applying this RR to asthma prevalence. CREAHIA also has separate incidence and prevalence EPI metrics.

### Decision requested

Should L02 be excluded from the new Registry? If prevalence is needed later, should it use a separate evidence-backed CRF?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Retire L02 | Do not create a Registry row that appears evidence-backed but lacks prevalence provenance. Keep it only for legacy fingerprint comparison. |
| B. Add a separate prevalence CRF | Ask HIA reviewers to nominate or approve a source, then add it as an explicit scientific change. |

**References:** [Khreis et al. 2017](https://doi.org/10.1016/j.envint.2016.11.012); [Achakulwisut et al. 2019](https://doi.org/10.1016/S2542-5196(19)30046-4)

## Q03 How should birth-outcome ORs and uncertainty be represented?

**Affected legacy rows:** L07 PTB; L08 LBW.

### Legacy rows covered by this question

- **L07 — PM2.5 / PTB / PTB:** Trasande's base-case effect measure is an OR; the legacy low/high values are scenarios, not a CI.
- **L08 — PM2.5 / LBW / LBW:** Dadvand reports an OR for term LBW; confirm whether the legacy LBW EPI metric is term LBW.

### Confirmed evidence

- Trasande et al. 2016 reports a PTB base-case OR of 1.15 per 10 ug/m3. The legacy values 1.07 and 1.16 are low/high scenarios, not confidence bounds.
- Dadvand et al. 2013 reports an OR of 1.10 (1.03–1.18) per 10 ug/m3 for term LBW.
- The current log-linear PAF calculation treats these fields as RRs and uncertainty bounds. An OR is not generally interchangeable with an RR, and a scenario range should not be presented as a CI.

### Decision requested

How should the Registry record the source effect measure and Trasande scenario range? Is the legacy LBW EPI metric actually term LBW?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Preserve historical numbers only | Clearly label OR/RR and scenario limitations; use only for historical comparison. |
| B. Add effect-measure and scenario metadata | Preserve the original measure and add an approved conversion rule and scenario fields. |
| C. Use an RR source | Select evidence reported directly as an RR. This is an intentional scientific change and needs a documented rationale. |

**References:** [Trasande et al. 2016](https://doi.org/10.1289/ehp.1510810); [Dadvand et al. 2013](https://doi.org/10.1289/ehp.1205575)

## Q04 How should work-absence age, exposure duration, and double counting be preserved?

**Affected legacy row:** L09, `PM2.5 / Absences / Absences`.

### Legacy row covered by this question

- **L09 — PM2.5 / Absences / Absences:** HRAPIE's work-days-lost CRF applies to ages 20–65, while legacy EPI, output labels, and forecast scaling use different age definitions.

### Confirmed evidence

- WHO HRAPIE recommends the work-days-lost CRF for working people aged 20–65, with a two-week PM2.5 average converted to an annual average. It is a short-term Group B* impact.
- HRAPIE requires work days lost to be subtracted from restricted activity days before combining burdens. Legacy `double_counted = FALSE` does not mean that subtraction has been implemented.
- The legacy EPI baseline uses a 15–64 population share times a 15+ labour-force participation rate; output and forecasts use 25+. These are not the same population definition.

### Decision requested

Should ages 20–65 remain consistent from CRF through EPI, output, and forecast? How should the two-week-to-annual exposure interpretation be represented? If RAD and work days lost are both reported, how should the required subtraction be implemented?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Build a source-consistent 20–65 path | Define/rebuild a 20–65 baseline, output, and forecast; document any difference from legacy results. |
| B. Keep the old approximation only in historical configuration | Preserve the 15–64 × 15+ result for comparison; use a source-consistent definition in the new Registry. |
| C. Do not combine RAD and work days lost | Until subtraction can be implemented safely, prohibit their combined burden or report them separately. |

**References:** [WHO HRAPIE report](https://iris.who.int/handle/10665/329636); [World Bank 15–64 population metadata](https://databank.worldbank.org/metadataglossary/health-nutrition-and-population-statistics/series/SP.POP.1564.TO.ZS)

## Q05 Can long-term O3 respiratory mortality be represented as COPD deaths and YLLs?

**Affected legacy rows:** L10 COPD Deaths; L11 COPD YLLs.

### Legacy rows covered by this question

- **L10 — O3_8h / COPD / Deaths:** applies a respiratory-mortality HR to COPD deaths.
- **L11 — O3_8h / COPD / YLLs:** applies the same HR to COPD YLLs; beyond outcome mapping, the YLL application itself needs approval.

### Confirmed evidence

- Turner et al. 2016 reports respiratory mortality at ages 30+ using annual average daily maximum 8-h O3, adjusted for near-source PM2.5, regional PM2.5, and NO2.
- Malley et al. 2017 uses this HR for respiratory-mortality burden. COPD is a subset, approximately 70–71% of estimated respiratory mortality, not a COPD-specific RR.
- Legacy instead applies the PAF to COPD deaths and YLLs at ages 25–99 with a 35 ppb counterfactual; these settings differ from the source/model.

### Decision requested

Should the new Registry represent a source-faithful respiratory-mortality CRF, or retain COPD mapping only in a historical configuration? Is use for COPD YLLs scientifically approved?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Source-faithful respiratory-mortality CRF | Retain age 30+, exposure metric, and cutoff semantics; do not call it a COPD CRF. |
| B. COPD mapping for historical configuration only | Retain historical values and limitations, but not as a new general scientific definition. |
| C. Find a COPD-specific CRF | If COPD is required, select evidence directly supporting that outcome. |

**References:** [Turner et al. 2016](https://doi.org/10.1164/rccm.201508-1633OC); [Malley et al. 2017](https://doi.org/10.1289/EHP1390)

## Q06 How should long-term NO2 mortality coexist with the PM2.5 NCD.LRI aggregate?

**Affected legacy rows:** L12 NCD.LRI Deaths; L13 NCD.LRI YLLs.

### Legacy rows covered by this question

- **L12 — NO2 / NCD.LRI / Deaths:** applies HRAPIE's all-natural-cause mortality CRF to NCD.LRI deaths with a historical PM2.5-overlap adjustment.
- **L13 — NO2 / NCD.LRI / YLLs:** applies the same source to NCD.LRI YLLs; it is not a direct NCD.LRI-YLL CRF.

### Confirmed evidence

- WHO HRAPIE specifies annual NO2, all-natural-cause mortality, age 30+, and a threshold above 20 ug/m3, with overlap-aware Group B* treatment alongside PM2.5 mortality.
- Legacy uses NCD.LRI deaths/YLLs and reduces central/lower excess risk by one third. It retains the 20 ug/m3 threshold, but this adjustment is not the published HRAPIE CRF.
- `NCD.LRI` is a package aggregate. It cannot by itself show that the all-natural-cause HR applies, nor establish YLL or double-counting rules.

### Decision requested

Should the new Registry use a source-faithful all-natural-cause CRF at age 30+, or retain the legacy NCD.LRI mapping only as an explicitly labelled historical configuration? How should YLL and PM2.5 overlap be handled in either case?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Source-faithful all-natural-cause CRF | Use the HRAPIE outcome, age 30+, threshold, and Group B* overlap rule. |
| B. Legacy NCD.LRI for historical configuration only | Preserve the old adjustment and results, but state that it is not the original HRAPIE CRF. |
| C. Do not publish this mortality combination yet | Exclude it from a new public Set until outcome, YLL, and overlap rules are approved. |

**Reference:** [WHO HRAPIE report](https://iris.who.int/handle/10665/329636)

## Q07 Can long-term SO2 all-cause mortality be represented as NCD.LRI deaths and YLLs?

**Affected legacy rows:** L14 NCD.LRI Deaths; L15 NCD.LRI YLLs.

### Legacy rows covered by this question

- **L14 — SO2 / NCD.LRI / Deaths:** applies Krewski's all-cause mortality HR to NCD.LRI deaths.
- **L15 — SO2 / NCD.LRI / YLLs:** applies the same HR to NCD.LRI YLLs; the source does not directly support this YLL application.

### Confirmed evidence

- Krewski et al. 2009 is all-cause mortality in a 30+ cohort, with SO2 exposure measured as the 1980 monitoring average. The legacy RR per 5 ppb matches the paper's MSA and DIFF specification.
- Legacy applies the RR to NCD.LRI deaths/YLLs, has no age bounds in the default file, and uses a 0.02 ppb counterfactual and ug/m3-to-ppb conversion.
- The source does not define NCD.LRI, ages 25–99, a near-zero counterfactual, or a YLL application.

### Decision requested

Should the new Registry use a source-faithful all-cause SO2 CRF at age 30+, or retain NCD.LRI/AllCause mappings only as historical configurations? Are YLL, counterfactual, and unit conversion approved?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Source-faithful all-cause CRF | Clearly retain age 30+, SO2 exposure definition, unit, and counterfactual. |
| B. Historical mapping only | Do not treat historical NCD.LRI/AllCause and YLL usage as a new general scientific CRF. |
| C. Wait for approval of YLL and aggregate rules | Avoid creating a Registry row that looks complete but lacks decision support. |

**Reference:** [Krewski et al. 2009 HEI report](https://www.healtheffects.org/system/files/Krewski140.pdf)

## Q08 How should the Registry record effect-estimate and HIA-implementation provenance?

**Affected Registry row:** L01, `legacy_no2_asthma_1to18_incidence_v1`.

### Legacy row covered by this question

- **L01 — NO2 / Asthma.1to18 / AsthmaIncidence:** Khreis is the effect-estimate source; Achakulwisut is the source for the 1–18 population and 2 ppb HIA implementation. One `reference_id` cannot express both roles.

### Confirmed evidence

- The current schema has one `reference_id`. L01 uses Khreis et al. 2017 because it is the effect-estimate source.
- Achakulwisut et al. 2019 documents the HIA population of ages 1–18 and the 2 ppb counterfactual. It exists in `references.csv`, but no Registry row uses it.
- Keeping only Khreis hides HIA settings in notes; replacing it with Achakulwisut hides the association source. A multi-value `reference_id` would break the current single-value meaning used by search, preview, and override.

### Decision requested

Does the Registry need machine-readable provenance roles? If yes, should the smallest schema change be an `implementation_reference_id`, or a scalable provenance table?

### Options

| Option | Meaning and consequence |
| --- | --- |
| A. Add `implementation_reference_id` | Smallest schema change: retain one effect-estimate and one implementation reference. |
| B. Create a provenance table | Permit several references per CRF with a role; more complete but larger implementation scope. |
| C. Keep provenance in documentation temporarily | Keep the Registry row provisional until a future schema change can make provenance machine-readable. |

**References:** [Khreis et al. 2017](https://doi.org/10.1016/j.envint.2016.11.012); [Achakulwisut et al. 2019](https://doi.org/10.1016/S2542-5196(19)30046-4)

## Next steps after decisions

1. Record every decision in the migration review and decision log.
2. Add only CRFs with approved scientific definitions to the Registry; label historical-reproduction-only rows clearly.
3. For every intentional parameter change, update fingerprint comparison with an authorised difference and its rationale.
4. Once all 15 default rows have an approved reference, age, outcome, and overlap definition, create the complete Registry selection and run HIA-level fingerprint parity.

## Related technical documents

- [Historical configuration mapping](../.github/prompts/dev/rr_crf_redesign/09-historical-config-mapping.md): complete historical configuration mapping and PAF/HIA fingerprint strategy across default, C40, and Krewski–South Africa profiles, as well as RR/EPI/population settings.
- [Legacy CRF migration manifest](../.github/prompts/dev/rr_crf_redesign/08-legacy-crf-migration-manifest.md): technical migration inventory of 25 CRF definitions and 55 active source-row memberships.
- [RR/CRF redesign roadmap](../.github/prompts/dev/rr_crf_redesign/00-roadmap.md): overall redesign direction, breaking-change boundary, and planned test work.
