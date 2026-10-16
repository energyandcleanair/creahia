# Phase 1 Release Readiness

## Purpose

This document records release evidence for Phase 1 of the RR/CRF redesign. It
separates completed numerical and compatibility checks from the remaining
package, maintenance, and release decisions.

Passing the checks below means that the tested legacy and Registry
configurations remain numerically compatible. It does not approve unresolved
HIA methodology and does not by itself mark Phase 1 complete.

## Evidence snapshot

- Date: 2026-10-16
- Branch: `feat/unify_rr_and_crf`
- Commit: `8e9f12d`
- Required compatibility contracts: 14 of 14 `registry_ready`
- Ordinary supported legacy calls: Registry routing enabled by default
- Explicit legacy comparison override: retained pending discussion with Hubert

## Release-level fingerprint sweep

The focused release sweep was run with:

```r
devtools::test(
  filter = "fingerprint|parity|compatibility|contract",
  stop_on_failure = TRUE
)
```

Result:

- 1,165 expectations passed
- 0 failures
- 0 test warnings
- 0 skipped tests
- the only top-level environment message reported a missing `crea_api_key`;
  the deterministic release checks completed without it

The sweep covers the actual package default, the default, C40, and
Krewski-South Africa historical CRF profiles, all nine historical
RR/EPI/population configurations, fixed snapshots, live legacy-versus-Registry
comparisons, wrapper contracts, and direct RR source-family contracts.

The cross-version result check was run separately with:

```r
devtools::test(
  filter = "results_immutability",
  stop_on_failure = TRUE
)
```

Result:

- 2 expectations passed
- 0 failures
- 3,984 historical missing-result cases were recognised as authorised
- 0 unauthorised missing-result cases
- 608 historical numerical changes were recognised as authorised
- 0 unauthorised numerical changes
- 10 environment warnings: one missing local `.Renviron` warning and repeated
  missing `crea_api_key` warnings while the current package was reloaded

No fixed snapshot was changed. Generated current-version CSV files were removed
after the comparison, and test-generated diagnostic images were restored to
their committed versions.

## What this evidence closes

- The current compatibility matrix and required call contracts have release-run
  numerical evidence.
- Fixed snapshots and live legacy-versus-Registry comparisons both pass.
- No unexplained numerical change was accepted or written into a fixture.
- Existing Diabetes PAF ordering warnings remain reproducible; their scientific
  interpretation remains an Open Questions item rather than an engineering
  compatibility failure.

## Remaining Phase 1 gates

- Run the broader package test suite and formal package check on the release
  candidate, and document every unrelated pre-existing warning or failure.
- Consolidate known limitations and authorised differences into a reviewable
  record.
- Finalise the installation and rollback options without choosing the release
  vehicle prematurely.
- Discuss the Hubert maintainer runbook and root `AGENTS.md` changes after the
  other release evidence is complete, following the 2026-10-12 mentor feedback.
- Decide with Hubert whether to remove the explicit
  `creahia.hia_compatibility_routing = FALSE` comparison route.
- Choose the release vehicle and obtain explicit approval before merge or
  cutover.
