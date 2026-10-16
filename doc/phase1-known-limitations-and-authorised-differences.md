# Phase 1 Known Limitations and Authorised Differences

## Purpose

This document is the reviewable Phase 1 record of:

- numerical differences that are explicitly recognised by the cross-version
  compatibility test;
- limitations that remain visible after engineering compatibility is complete;
- boundaries between historical reproduction and scientific approval; and
- decisions that remain with the HIA team or Hubert.

It should be read with the
[Phase 1 release-readiness record](phase1-release-readiness.md) and the
[HIA methodology open questions](hia-methodology-validation-open-questions.md).

## Current Phase 1 numerical conclusion

The Phase 1 Registry rollout introduces **no authorised numerical difference**
for an equivalent current-package configuration.

- The focused fingerprint, parity, compatibility, and contract sweep passed
  1,165 expectations without failures.
- The full development suite passed 3,092 expectations without failures.
- The installed-package suite passed 3,082 expectations without failures.
- No fixed fingerprint was changed to make the new route pass.
- No unexplained difference was accepted.

The authorised differences below belong to comparisons between older released
package versions. They are not permission for the Registry route to differ
from the equivalent legacy route.

## Authorised historical differences

`tests/testthat/test_results_immutability.R` recognises five narrowly defined
historical categories:

| Scope | Recognised difference | Reason recorded in the test |
| --- | --- | --- |
| Versions 0.4.1 and 0.4.4, GBD 2019, `GEMM and GBD` | COPD, IHD, LC, LRI, and Stroke YLL rows were unavailable in the older results and appear in later versions | The older YLL values were `NA` |
| Versions 0.4.1–0.5.1 | GBD 2021 configurations are absent | Those versions pre-date GBD 2021 support |
| Versions 0.4.1–0.7.1 | GBD 2023 configurations are absent | Those versions pre-date GBD 2023 support |
| Change introduced at version 0.5.1 | PTB, LBW, and Absences values may change | Epidemiological data update |
| Change introduced at version 0.6.1 | Values for all represented causes may change by less than 1% | Population projection data update |

In the 2026-10-16 release sweep these rules accounted for 3,984 historical
missing-result observations and 608 historical numerical changes. The test
found zero unauthorised missing results and zero unauthorised numerical
changes. These counts are row-level comparison observations, not 4,592 separate
methodology decisions.

Any future difference must remain narrow. It needs a named cause, affected
version or configuration, expected rows or values, supporting review evidence,
and an explicit test rule. A fixture must not be updated merely because both
implementations changed together.

## Known scientific and data limitations

### Historical reproduction is not scientific approval

Historical CRF and RR definitions with unresolved methodology remain internal
and marked `not_reviewed`. Registry routing proves that the package can
reproduce the historical calculation through the shared engine; it does not
make those definitions recommended scientific choices.

The twelve current methodology questions are maintained in
`doc/hia-methodology-validation-open-questions.md`. They cover age and outcome
meaning, incidence versus prevalence, effect-measure interpretation, exposure
and counterfactual assumptions, cross-outcome curve reuse, overlapping disease
groups, double counting, and the future recommended default.

### Diabetes uncertainty ordering

The original-source Diabetes Deaths and YLDs path can produce low, central, and
high PAF estimates that are not properly ordered or have inconsistent signs.
Both legacy and Registry routes reproduce the same warning. This is useful
compatibility evidence but does not resolve the uncertainty interpretation.
The methodology question remains Q08.

### Korea C40 child-LRI omission

The portable Korea C40 calculation omits child LRI because the required Korea
C40 age weights are unavailable. Both routes reproduce that omission. The HIA
team still needs to decide whether to provide a reviewed age-weight source or
formally declare that result unavailable. The methodology question remains
Q09.

### External project inputs

The portable C40 dual run uses packaged CRFs, packaged Korea C40 EPI data, and
deterministic exposure inputs. The original Korea Coal CALPUFF rasters remain
on an external project drive and were not converted into package test data.
The portable result is representative package-level evidence, not a rerun of
that original project dataset.

## Known engineering and release limitations

### Temporary comparison override

`creahia.hia_compatibility_routing = FALSE` remains available as a temporary
comparison escape hatch. Ordinary supported calls route through the Registry
by default, but Phase 1 cannot claim the final single-engine exit condition
until Hubert and Miao decide whether and when to remove this override.

### Package-check technical debt

The formal installed-package check has zero errors but retains nine warnings
and seven notes. They concern existing package hygiene, including executable
and non-portable files, dependency and namespace declarations, S3 signatures,
documentation coverage, the `data/epi_update` layout, test dependencies,
metadata, development directories, startup output, and static global-binding
analysis. They do not indicate a failed RR/CRF parity check, but they should be
handled as separate maintenance work rather than silently forgotten.

### Environment-only warnings

Local validation can report a missing `.Renviron`, missing `crea_api_key`, or
font lookup/download warnings. The deterministic release checks do not require
the API key or a downloaded font. These messages must not be confused with
accepted numerical differences.

## Remaining decisions and owners

| Item | Current boundary | Decision owner | Phase 1 effect |
| --- | --- | --- | --- |
| Q01–Q12 methodology questions | Keep unresolved definitions historical-only and internal | HIA team, organised through the Open Questions review | Does not block engineering reproduction; blocks scientific endorsement or a future recommended default |
| Explicit `FALSE` comparison override | Retain temporarily | Hubert and Miao | Blocks the final single-engine exit claim |
| Release vehicle | Do not choose between isolated trial installation and direct shared-package update yet | Hubert and Miao | Blocks deployment/cutover |
| Installation and rollback procedure | Document both options before selection | Miao and Codex can prepare; Hubert reviews the operational choice | Blocks operational handover |
| Hubert maintainer runbook and root `AGENTS.md` | Defer until the evidence and operational options are complete | Hubert and Miao | Blocks final leave-period handover |

## Review rule

Engineering reproduction, scientific approval, and release approval must be
reported separately:

1. **Engineering reproduction:** equivalent legacy and Registry
   configurations produce the protected outputs and observable behaviour.
2. **Scientific approval:** the HIA team confirms that the source, outcome,
   age, counterfactual, uncertainty, and overlap assumptions are appropriate.
3. **Release approval:** Hubert and Miao choose the deployment vehicle,
   rollback procedure, and final comparison-override policy.

Passing one layer must not be described as passing the next.
