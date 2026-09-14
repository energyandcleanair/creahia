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

#### Legacy CRF and counterpart metadata

| Legacy row | Legacy cause key | C40/South-Africa metadata | Legacy impact label |
| --- | --- | --- | --- |
| L03, L05 | `Asthma.0to17` | `0–18` (`age_low = 0`, `age_high = 18`) | `0–18` |
| L04, L06 | `Asthma.18to99` | `18–99` (`age_low = 18`, `age_high = 99`) | `25+` |

These are three different historical representations. Their similar names are
not sufficient evidence that they mean the same population.

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
3. the final age group shown in the HIA output.

For L03–L06, the current evidence shows that these concepts may differ. If the
Registry needs to preserve more than one of them, separate metadata may be
needed rather than treating a single pair of bounds as all three.

### Decision requested

Define the semantic contract of Registry `age_low` and `age_high` for
log-linear CRFs, and decide whether another field is needed to preserve study
population, EPI-routing, or output-label information separately.

### Status

`OPEN — continue evidence recovery and EPI tracing; defer scientific/semantic
decision to the consolidated review.`

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
