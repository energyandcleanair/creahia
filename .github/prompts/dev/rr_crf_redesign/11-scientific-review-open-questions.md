# Scientific Review: Open Questions

## Purpose

This document is the review queue for questions that cannot be resolved from
the legacy code, packaged data, or the original epidemiological reference
alone. It is deliberately separate from the migration readiness audit:

- `10-default-profile-migration-readiness-audit.md` answers: *what is the
  migration state of each legacy row, and what evidence is still missing?*
- this document answers: *after that evidence has been gathered, what
  scientific or semantic decision still requires review?*

The goal is to keep reference recovery moving while collecting a short,
evidence-backed review pack for Hubert and HIA-domain colleagues. An open
question is not permission to guess a Registry value or to add a partially
defined public CRF.

## Triage rules

| Type of uncertainty | Action now | Record here? |
| --- | --- | --- |
| Evidence gap | Continue source recovery or data inspection. | Only if a decision remains after the evidence is recovered. |
| Engineering question | Trace the code and data routing. | Link the trace when it informs a later decision. |
| Scientific or semantic decision | Preserve the alternatives and their consequences. | Yes. |
| Historical intent cannot be recovered | Do not infer it from a name alone. | Yes, after reasonable code and source checks. |

Each entry distinguishes confirmed evidence from the decision being requested.
This prevents an open question from being mistaken for a factual claim about a
paper or the legacy calculation.

## Q01 — What should Registry age bounds mean for asthma ERV CRFs?

**Affected legacy rows:** L03–L06 in `legacy_profile_default`.

**Review timing:** keep this question open while reference recovery continues.
Review it together with the later scientific-review pack; it must be resolved
before L03–L06 become final Registry rows or enter a historical Set.

### Confirmed evidence

#### Epidemiological source

- L03–L06 are traced to Zheng et al. (2015),
  `zheng_2015_asthma_erv`.
- The paper reports broad subgroup categories such as children, adults, and
  elderly. It does not define CREAHIA's exact numeric boundaries such as
  `0–17`, `0–18`, `18–99`, or `25+`.
- The original study therefore supports RR provenance, but does not by itself
  define the Registry age metadata.

#### Effect-estimate-to-age-label mismatch

- L03 matches Zheng's PM2.5 **child subgroup** RR `1.025` (`1.013–1.037`)
  per `10 ug/m3`.
- L05 reverses to approximately O3 RR `1.009` (`1.005–1.012`) per
  `10 ug/m3` after the legacy unit conversion. Zheng's published overall O3
  result is `1.009` (`1.006–1.011`), but its textual child subgroup summary
  does not report an O3 estimate. The original table or supplementary data has
  not yet been verified to establish L05's parameter provenance. Do not label
  L05 as a child subgroup estimate until that evidence is recovered.
- In contrast, L04 is labelled `Asthma.18to99` but matches Zheng's PM2.5
  **overall** RR `1.023` (`1.015–1.031`) per `10 ug/m3`, rather than an adult
  subgroup estimate.
- L06 is likewise labelled `Asthma.18to99` but matches Zheng's **overall** O3
  RR after conversion from `10 ug/m3` to `10 ppb`: `1.018`
  (`1.012–1.022`). It is not an adult subgroup estimate.
- Therefore, Q01 is not only an `age_low` / `age_high` contract question.
  L04 and L06 also require a decision on whether an overall-age effect
  estimate may be retained under an adult-labelled HIA/EPI route, while L05
  requires parameter-provenance recovery before its age semantics can be
  reviewed.

#### Legacy CRF and counterpart metadata

| Legacy row | Legacy cause key | C40/South-Africa metadata | Legacy impact label |
| --- | --- | --- | --- |
| L03, L05 | `Asthma.0to17` | `0–18` (`age_low = 0`, `age_high = 18`) | `0–18` |
| L04, L06 | `Asthma.18to99` | `18–99` (`age_low = 18`, `age_high = 99`) | `25+` |

These are three different historical representations. Their similar names are
not sufficient evidence that they mean the same population.

`CRFs_C40.csv` also contains an Excel-style display corruption: the two
`Asthma.1to18` rows show `age_group = 18-Jan`, while their numeric
`age_low = 1` and `age_high = 18` values remain intact. Treat `age_group` as
non-authoritative display metadata until that source file is repaired and its
consumers are checked.

#### Confirmed engineering behaviour

- `R/hia_data.R::fix_epi_cols()` converts legacy EPI column names such as
  `exac.0to17` and `exac.18to99` into metric keys matching the legacy asthma
  cause/outcome names.
- `R/multiply_paf_epi.R::multiply_paf_epi()` joins PAF rows to EPI data using
  that cause/outcome metric key. The legacy cause key therefore controls which
  EPI metric is selected.
- `R/compute_hia_utils.R::add_age_group()` assigns `0–18` to causes named
  `0to17` or `1to18`, and assigns `25+` to most remaining causes. This is an
  output-label rule; it does not prove the literal age coverage of the EPI
  metric used in the calculation.
- In the current Registry log-linear path,
  `R/crf_apply.R::apply_crf_log_linear()` calculates PAF from the CRF numeric
  fields and concentration/population inputs. It does not read
  `age_low` or `age_high`. Those fields are descriptive metadata in this path
  today, not calculation-routing controls.

### Engineering follow-up still to do

Trace the source construction of the `exac.0to17` and `exac.18to99` EPI
metrics far enough to document their literal age coverage. This is an evidence
task for the implementation team, not a question to send to reviewers yet.

### Why a review decision is needed

One pair of Registry fields cannot safely stand for several different concepts
without an explicit contract. `age_low` / `age_high` could mean:

1. the study population described by the paper;
2. the population/EPI metric to which CREAHIA applies the CRF; or
3. the final age group shown in the HIA output; or
4. an age label inherited from historical routing despite an overall-age
   epidemiological effect estimate.

For L03–L06, the current evidence shows that these concepts may differ. If the
Registry needs to preserve more than one of them, separate metadata may be
needed rather than treating a single pair of bounds as all three.

### Decision requested

After L05's parameter source is recovered, define the semantic contract of
Registry `age_low` and `age_high` for log-linear CRFs, and decide whether
another field is needed to preserve study population, EPI-routing, or
output-label information separately.

### Status

`OPEN — recover L05's O3 parameter source and continue EPI tracing; defer
scientific/semantic decision to the consolidated review.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `inst/extdata/rr/processed/CRFs_Krewski_SouthAfrica.csv`
- `R/hia_data.R::fix_epi_cols()`
- `R/multiply_paf_epi.R::multiply_paf_epi()`
- `R/compute_hia_utils.R::add_age_group()`
- `R/crf_apply.R::apply_crf_log_linear()`
- Zheng X-Y, Ding H, Jiang L-N, et al. (2015),
  doi:10.1371/journal.pone.0138146

## Q02 — Should the legacy NO2 asthma-prevalence row be retired?

**Affected legacy row:** L02, `NO2 / Asthma.1to18 / AsthmaPrevalence`.

**Review timing:** this question is ready for the consolidated scientific
review. It must be resolved before L02 is migrated, selected in a historical
Set, or used in a new Registry workflow.

### Confirmed evidence

- L01 and L02 have identical RR, counterfactual, unit, multiplier, and age
  metadata: RR 1.26 (1.10–1.37) per 10 ppb, 2 ppb counterfactual, and ages
  1–18.
- `git blame` shows that the duplicate L02 row was introduced with the L01 row
  in the September 2025 cause/outcome restructuring commit. The active default,
  C40, and South-Africa files retain the duplication.
- The original meta-analysis recorded for L01, Khreis et al. (2017), concerns
  the development of childhood asthma. It is not a prevalence CRF.
- Achakulwisut et al. (2019) documents the historical HIA application of the
  same RR: it applies the 1.26 (1.10–1.37) per 10 ppb association to **new
  paediatric asthma incidence** for children aged 1–18 years. It also defines
  the 2 ppb counterfactual as the fifth percentile of the minimum exposure
  concentrations in the studies included by Khreis.
- The package has separate EPI metrics for `Asthma.Inci.1to18` and
  `Asthma.Prev.1to18`; prevalence is therefore not merely a display alias for
  the incidence metric.

### What has not been found

No original study or methodology source has been found that supports applying
this particular RR to asthma prevalence. The identical L02 parameters are not
evidence of a valid prevalence response function.

### Why a review decision is needed

The breaking-change Registry must not create a scientific-looking prevalence
row just because the legacy workflow contained one. There are two materially
different outcomes:

1. **Retire L02 from the Registry migration.** Keep it in the historical audit
   and fingerprint comparison only as a legacy behaviour that cannot be
   represented as a supported scientific CRF.
2. **Adopt a separate, evidence-backed prevalence CRF.** This would require a
   source selected by HIA-domain reviewers and would be a deliberate scientific
   change, not a direct historical migration.

### Decision requested

Confirm whether L02 should be retired from the redesigned Registry rather than
migrated. If a prevalence outcome is required in future, nominate or approve a
separate source and document the resulting numerical change explicitly.

### Status

`OPEN — legacy parameter provenance is traced; no supported prevalence CRF has
been identified.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `inst/extdata/rr/processed/CRFs_Krewski_SouthAfrica.csv`
- `inst/extdata/epi/processed/epi_rate_wide_gbd2019.csv`
- `R/hia_data.R::fix_epi_cols()`
- Khreis H, Kelly C, Tate J, et al. (2017),
  doi:10.1016/j.envint.2016.11.012
- Achakulwisut P, Brauer M, Hystad P, Anenberg SC. (2019),
  doi:10.1016/S2542-5196(19)30046-4

## Q03 — How should source odds ratios be represented in the log-linear Registry?

**Affected legacy rows:** L07 (`PM2.5 / PTB`) and L08 (`PM2.5 / LBW`).

**Review timing:** source recovery is complete enough to record the question.
Resolve it before adding the birth CRFs as final Registry rows. The existing
legacy numbers must still be retained for later historical fingerprint testing.

### Confirmed evidence

- Trasande et al. (2016) reports a base-case **odds ratio** of `1.15` per
  `10 ug/m3` and a reference level of `8.8 ug/m3` for PM2.5 preterm birth.
  The `1.07` and `1.16` values are its low- and high-end **scenario** inputs,
  not a confidence interval. L07 stores all three values in fields named
  `rr_low`, `rr_central`, and `rr_high`.
- Dadvand et al. (2013) reports an **odds ratio** of 1.10 (1.03–1.18) per
  10 ug/m3 for PM2.5 and **term** low birth weight. These exactly match L08's
  numerical parameters; the legacy 4 ug/m3 counterfactual is not stated in
  that paper.
- `R/crf_apply.R::apply_crf_log_linear()` uses `rr_low`, `rr_central`, and
  `rr_high` as relative risks in the standard log-linear PAF formula. The
  legacy log-linear path uses the same interpretation.
- Trasande et al. describes converting the input OR to an RR for its US burden
  calculation, but the legacy CRF stores the reported 1.15 value unchanged.

### Why a review decision is needed

An odds ratio and a relative risk are not generally interchangeable. Calling an
OR an RR in Registry metadata could overstate or misstate the intended effect,
while silently converting it would change legacy numerical results.

The Trasande low/high scenarios are a separate issue from the effect measure.
The existing `rr_low` / `rr_high` fields otherwise represent effect-estimate
uncertainty bounds. Reusing them for scenario values would make a sensitivity
range look like a confidence interval unless the Registry records its meaning.

L08 has a second semantic boundary: its recovered evidence is for **term** LBW,
so the reviewers should also confirm that the legacy `LBW` EPI metric represents
the intended outcome population.

### Decision requested

Define the supported treatment for source ORs in log-linear CRFs:

1. preserve the historical numeric interpretation only in a clearly labelled
   historical configuration, with a documented limitation;
2. add explicit effect-measure metadata and a scientifically approved
   conversion/implementation rule; or
3. select a different evidence source expressed as an RR.

Separately, define how the Registry represents a low/high scenario range that
is not an uncertainty interval. It may require explicit scenario metadata or a
historical-only representation rather than populating generic `rr_low` /
`rr_high` fields without qualification.

Also confirm whether the L08 baseline EPI outcome is term LBW. Any intentional
change from legacy numerical results must be documented as an authorised
fingerprint difference rather than silently introduced during migration.

### Status

`OPEN — RR provenance is recovered; effect-measure and term-LBW semantics need
scientific/architecture review.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `R/crf_apply.R::apply_crf_log_linear()`
- Trasande L, Malecha P, Attina TM. (2016), doi:10.1289/ehp.1510810
- Dadvand P, Parker J, Bell ML, et al. (2013), doi:10.1289/ehp.1205575

## Q04 — How should the work-absence age population be preserved through output and forecasting?

**Affected legacy row:** L09, `PM2.5 / Absences / Absences`.

**Review timing:** source recovery is complete. Complete the listed engineering
trace, then decide the Registry/output contract before migrating L09. Do not
silently change historical fingerprints while resolving it.

### Confirmed evidence

- WHO HRAPIE (2013) recommends the PM2.5 work-days-lost CRF as RR 1.046
  (1.039–1.053) for the working-age population aged 20–65 years. It describes
  a two-week PM2.5 average converted to an annual average, and classifies the
  impact as short-term Group B*.
- HRAPIE explicitly says that estimated work days lost should be subtracted
  from estimated restricted activity days (RADs) before burden calculations to
  avoid double counting. A bare legacy `double_counted = FALSE` flag does not
  state or implement this subtraction rule.
- The legacy C40 and South-Africa rows preserve that population explicitly as
  `age_low = 20`, `age_high = 65`; L09 uses the same RR and counterfactual 0
  in all active legacy profiles.
- `R/generate_epi.R` creates the `Absences` EPI metric using a labour-age share
  and labour-force-participation percentage. Thus the baseline burden is not a
  generic all-age rate.
- `R/compute_hia_utils.R::add_age_group()` has no Absences-specific case and
  assigns the final legacy impact row the fallback label `25+`.
- `R/econ_computation.R` has distinct population-scaling rules for `20-65` and
  `25+`, and applies them by the final HIA `age_group`. A work-absence result
  labelled `25+` therefore uses the wrong age-group scaling in forecasts.

### Engineering follow-up still to do

Trace the construction of `labor_age_share_pct` to verify that its exact age
range and working-population definition match HRAPIE's 20–65 population. This
is a code/data evidence task, not yet a reviewer question.

### Why a review decision is needed

The historical CRF metadata, EPI metric, displayed age group, and forecasting
population must describe the same population. They currently do not: the
source and CRF metadata say 20–65, while the output/forecast path says 25+.

The Registry must also preserve the source's exposure-duration semantics. A
generic `PM2.5` row alone does not state that the source recommendation is a
two-week average converted to annual-average PM2.5.

### Decision requested

Confirm the Registry and output contract for work absences:

1. preserve 20–65 as the applicable population through result labelling and
   forecasting; and
2. record how the required two-week-to-annual PM2.5 exposure interpretation is
   represented or constrained by the selected workflow.
3. record and, where RADs and work days lost are combined, implement the
   source-required RAD subtraction rule instead of treating
   `double_counted = FALSE` as sufficient evidence.

If correcting the output label changes a historical forecast, document it as
an authorised fingerprint difference with an explicit rationale.

### Status

`OPEN — CRF provenance is recovered; output/forecast age semantics and exposure
duration require engineering trace and scientific/architecture review.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `inst/extdata/rr/processed/CRFs_Krewski_SouthAfrica.csv`
- `R/generate_epi.R`
- `R/compute_hia_utils.R::add_age_group()`
- `R/econ_computation.R`
- WHO Regional Office for Europe (2013), HRAPIE project report

## Q05 — Can the long-term O3 respiratory-mortality CRF be represented as COPD deaths and YLLs?

**Affected legacy rows:** L10 and L11, `O3_8h / COPD / Deaths` and
`O3_8h / COPD / YLLs`.

**Review timing:** source recovery is complete. Resolve this before final
Registry rows or a historical Set is published. Preserve the legacy values for
fingerprint comparison until any intentional change is explicitly authorised.

### Confirmed evidence

- Turner et al. (2016) reports HR 1.12 (1.08–1.16) per 10 ppb for **respiratory
  mortality**, using annual average daily maximum 8-h O3. This is the numerical
  source of the legacy RR. The selected model is adjusted for near-source
  PM2.5, regional PM2.5, and NO2.
- Malley et al. (2017) uses that Turner HR to estimate long-term O3-attributable
  **respiratory mortality** in adults aged 30 years and older. It separately
  reports COPD mortality as a subset of respiratory mortality; it does not turn
  the Turner HR into a COPD-specific epidemiological estimate.
- In Malley's COPD-specific burden calculation, estimated O3-attributable COPD
  mortality is about 70–71% of its estimated O3-attributable respiratory
  mortality. This usefully quantifies outcome coverage, but is not a direct
  estimate of the bias from applying Turner's respiratory HR `1.12` to a COPD
  baseline.
- The main Turner-based Malley analysis uses a 26.7 ppb low-concentration
  cutoff, while the legacy CRFs use 35 ppb.
- The legacy C40/South-Africa rows label the population 25–99, while the
  Malley implementation is for adults aged 30 years and older.
- CREAHIA applies the legacy PAF to `COPD_Deaths` and `COPD_YLLs` EPI metrics,
  so the current calculation treats the respiratory-mortality HR as a
  COPD-specific effect for both deaths and years of life lost.

### Why a review decision is needed

The outcome, age range, and counterfactual are each scientifically meaningful.
None should be silently inherited merely because the RR number matches:

1. respiratory mortality is broader than COPD mortality;
2. the source/model population is 30+, not the legacy 25–99 metadata; and
3. 35 ppb is not the low-concentration cutoff used in Malley's main
   Turner-based calculation.
4. Turner’s multipollutant adjustment supports an overlap-aware interpretation
   when combined with PM2.5 and NO2 estimates, but it is not on its own proof
   that every Registry combination has no double counting.

The Registry also needs a clear contract for the O3 exposure metric: the source
is annual average daily maximum 8-h O3, whereas `O3_8h` alone does not state
the averaging period.

### Decision requested

Decide whether the redesigned Registry should:

1. represent the source transparently as a respiratory-mortality CRF with its
   source age/counterfactual/exposure semantics; or
2. retain the historical COPD mapping only as an explicitly documented legacy
   configuration, including its 25–99 and 35 ppb choices.

For either option, confirm whether a COPD YLL application is scientifically
approved. Any change to the legacy mapping must be declared as an authorised
fingerprint difference.

### Status

`OPEN — RR provenance is recovered; outcome, age, counterfactual, and exposure
metric semantics require scientific/architecture review.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `inst/extdata/rr/processed/CRFs_Krewski_SouthAfrica.csv`
- `inst/extdata/epi/processed/epi_rate_wide_gbd2019.csv`
- Turner MC, Jerrett M, Pope CA III, et al. (2016),
  doi:10.1164/rccm.201508-1633OC
- Malley CS, Henze DK, Kuylenstierna JCI, et al. (2017),
  doi:10.1289/EHP1390

## Q06 — How should the long-term NO2 mortality CRF coexist with the PM2.5 NCD.LRI aggregate?

**Affected legacy rows:** L12 and L13, `NO2 / NCD.LRI / Deaths` and
`NO2 / NCD.LRI / YLLs`.

**Review timing:** source recovery is complete. Resolve this before retaining
the existing L12 Registry placeholder, creating L13, or publishing a
historical mortality Set. Preserve the legacy values for fingerprint comparison
until an intentional change is explicitly authorised.

### Confirmed evidence

- WHO HRAPIE (2013) recommends an annual-mean NO2 long-term CRF for
  **all natural-cause mortality** in adults aged **30+**: RR `1.055`
  (`1.031–1.080`) per `10 ug/m3`, only above `20 ug/m3`.
- HRAPIE says this is a Group B* impact and calls for treatment that avoids
  possible overlap and double counting with the Group A* PM2.5 mortality
  estimate. Its outcome is not `NCD.LRI`.
- The legacy rows instead use `NCD.LRI_Deaths` and `NCD.LRI_YLLs`, omit age
  metadata, and store `1.037 (1.021–1.080)`. Historical CREA reports show the
  intended presentation as premature deaths at ages 30–99, and explain that
  the central and lower NO2 values were scaled down by one third to address
  possible overlap with PM2.5.
- The legacy values are numerically consistent with reducing the HRAPIE
  central and lower excess risks by one third (`1.055 -> 1.037` and
  `1.031 -> 1.021`); the upper value remains `1.080` in the legacy table.
  This is a CREA historical implementation convention, not the HRAPIE CRF as
  published.
- The legacy `counterfact = 20` and `unit = ug` values do preserve HRAPIE's
  20 ug/m3 concentration threshold. This is source-faithful evidence for the
  counterfactual only; it does not validate the legacy outcome, age, or overlap
  treatment.
- `R/compute_hia_utils.R::add_age_group()` gives the default fallback label
  `25+` to the legacy NCD.LRI rows. The active Registry L12 placeholder has
  `age_low = 25`, `age_high = Inf`, and temporary `legacy_default_crfs`
  provenance. None of these settings is evidence for the source population.
- `inst/extdata/crf/dc_groups.csv` currently classifies `NCD.LRI` as an
  aggregate containing IHD, Stroke, COPD, LC, LRI, Diabetes, and Dementia.
  That package taxonomy does not turn the HRAPIE all-natural-cause outcome into
  an NCD.LRI-specific estimate.

### Why a review decision is needed

The source and historical implementation make three separate choices that the
Registry must not silently merge:

1. **Outcome:** HRAPIE is all natural-cause mortality; legacy applies it to
   the narrower `NCD.LRI` EPI aggregate and then also to YLLs.
2. **Population:** HRAPIE is 30+, whereas the default file is silent and the
   current output/Registry placeholder uses 25+.
3. **Overlap handling:** HRAPIE requires a PM2.5-overlap-aware Group B*
   calculation; a bare `double_counted = FALSE` flag does not document or
   implement the historical one-third adjustment.

### Decision requested

Decide whether the redesigned Registry should:

1. represent a source-faithful all-natural-cause, age-30+, annual-NO2 CRF and
   specify how its Group B* PM2.5-overlap treatment is enforced; or
2. retain the historical `NCD.LRI` deaths/YLLs mapping and adjusted central /
   lower values only as an explicitly documented legacy configuration,
   including the age and overlap rationale.

For either option, confirm whether applying this mortality CRF to YLLs is
scientifically approved. Any change to the historical outcome, age group, or
overlap adjustment must be declared as an authorised fingerprint difference.

### Status

`OPEN — RR provenance is recovered; outcome, age, YLL application, and
PM2.5-overlap semantics require scientific/architecture review.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/dc_groups.csv`
- `R/compute_hia_utils.R::add_age_group()`
- WHO Regional Office for Europe (2013), HRAPIE project report,
  https://iris.who.int/handle/10665/329636
- CREA historical reports using the same legacy implementation: Philippines
  Coal Health Report (2021), and Payra Bangladesh case study (2020)

## Q07 — Can the long-term SO2 all-cause mortality CRF be represented as NCD.LRI deaths and YLLs?

**Affected legacy rows:** L14 and L15, `SO2 / NCD.LRI / Deaths` and
`SO2 / NCD.LRI / YLLs`.

**Review timing:** source recovery is complete. Resolve this before creating
Registry rows or publishing a historical mortality Set. Preserve the legacy
values for fingerprint comparison until an intentional change is explicitly
authorised.

### Confirmed evidence

- Krewski et al. (2009), Health Effects Institute Research Report 140, reports
  an all-cause mortality HR of `1.020` (`1.014–1.026`) for SO2 (1980), matching
  the legacy RR per `5 ppb`. The legacy value matches the source table's
  `MSA & DIFF` ecological-covariate specification.
- The ACS CPS-II cohort enrolled participants aged 30 years and older. Source
  exposure is the 1980 SO2 monitoring average assigned within each metropolitan
  area.
- The default rows apply that RR to `NCD.LRI_Deaths` and `NCD.LRI_YLLs`, omit
  age metadata, use a `0.02 ppb` counterfactual, and set
  `units_multiplier = 0.381679389` to convert the package's SO2 input from
  ug/m3 to the source increment in ppb.
- C40 and Krewski-South-Africa use the same RR/conversion but select
  `AllCause`, age 25–99. Historical CREA reports using the default value show
  premature deaths at ages 30–99.
- The source does not specify the legacy `NCD.LRI` aggregate, 25–99 metadata,
  near-zero counterfactual, or YLL application. `dc_groups.csv` is a package
  taxonomy and cannot establish any of those scientific meanings.

### Decision requested

Choose whether the Registry should represent a source-faithful all-cause,
age-30+, SO2 (1980) mortality CRF with explicit unit/counterfactual semantics,
or retain historical NCD.LRI / AllCause mappings only as documented legacy
configurations. In either case, confirm the YLL application and the required
aggregate/overlap treatment. Any intentional change needs an authorised
fingerprint difference.

### Status

`OPEN — RR provenance is recovered; outcome, age, YLL application,
counterfactual, and concentration-unit semantics require scientific/architecture
review.`

### Evidence locations

- `inst/extdata/rr/processed/CRFs.csv`
- `inst/extdata/rr/processed/CRFs_C40.csv`
- `inst/extdata/rr/processed/CRFs_Krewski_SouthAfrica.csv`
- `inst/extdata/crf/dc_groups.csv`
- `R/compute_hia_utils.R::add_age_group()`
- Krewski D, Jerrett M, Burnett RT, et al. (2009),
  Health Effects Institute Research Report 140,
  https://www.healtheffects.org/system/files/Krewski140.pdf

## Q08 — How should the Registry represent both effect-estimate and HIA implementation provenance?

**Affected Registry row:** `legacy_no2_asthma_1to18_incidence_v1` (L01).
This may become a general requirement for any CRF whose epidemiological effect
estimate and HIA implementation settings come from different sources.

**Review timing:** resolve before presenting L01 as a final, source-complete
public Registry CRF or using it as the template for further migrations. Do not
change the Registry schema until the provenance roles are agreed.

### Confirmed evidence

- The current Registry schema has exactly one `reference_id` column. Registry
  validation checks that this value exists in `references.csv`.
- L01 currently stores `reference_id = khreis_2017_childhood_asthma`. Khreis
  is the underlying epidemiological association source.
- Achakulwisut et al. (2019) is present in `references.csv` as
  `achakulwisut_2019_paediatric_asthma_incidence`, but no Registry row
  references it. It documents the HIA implementation's 1–18 population and
  2 ppb counterfactual.
- Current selection, search, preview, and Set override operations use
  `reference_id` as a single-valued identifier. Storing several IDs in that
  field would make those operations and their scientific meaning ambiguous.

### Why a review decision is needed

One field currently has to choose between two different facts:

1. **Effect-estimate provenance:** where the concentration-response
   association comes from; and
2. **Implementation provenance:** where an HIA-specific counterfactual,
   target population, unit conversion, or other operational setting comes
   from.

Keeping only Khreis makes the 2 ppb and 1–18 settings discoverable only in
free-text notes. Replacing it with Achakulwisut would hide the source of the
underlying association. A multi-value `reference_id` would overload a field
that currently identifies one scientific source for Registry operations.

### Decision requested

Confirm the provenance contract for Registry CRFs. Candidate approaches are:

1. add an optional, single-valued `implementation_reference_id` alongside the
   existing primary `reference_id`; **or**
2. introduce a normalised provenance table that records a CRF, reference, and
   role such as `effect_estimate` or `implementation`; **or**
3. keep multi-source provenance in planning documentation only and mark such
   Registry rows provisional until a future schema change.

The reviewers should decide which roles must be machine-readable for this
redesign. If an implementation reference is required, confirm whether it
governs only counterfactual/population metadata or may also cover unit
conversion and other calculation settings.

### Status

`OPEN — single-reference schema cannot yet express L01's two distinct
provenance roles without relying on notes.`

### Evidence locations

- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/references.csv`
- `R/crf_registry.R::CRF_REGISTRY_REQUIRED_COLUMNS`
- `R/crf_registry.R::validate_crf_registry()`
- `R/crf_select.R` (search, preview, and override operations)
- Khreis H, Kelly C, Tate J, et al. (2017),
  doi:10.1016/j.envint.2016.11.012
- Achakulwisut P, Brauer M, Hystad P, Anenberg SC. (2019),
  doi:10.1016/S2542-5196(19)30046-4
