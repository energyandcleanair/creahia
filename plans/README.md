# CREAHIA Maintainer Plans

- Last reviewed: 2026-10-19
- Verified against: `40602d4`

## Purpose

This directory is the shared entry point for maintainers and AI agents working
on the RR/CRF redesign and the resulting maintenance tasks. It contains the
current architecture and safe maintenance procedures, not the full history of
the refactor.

Start with this file after reading the root `AGENTS.md`. Use the code and tests
as the final source of truth. If this plan disagrees with the implementation,
stop and report the mismatch before changing either one.

## Current state

The package is in the compatibility-preserving Phase 1 architecture:

- ordinary supported legacy-shaped calls are resolved through the Registry-
  driven compatibility path;
- the established top-level compute flow remains
  `compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()`;
- historical configurations have parity and fixed-result protection;
- historical reproduction remains separate from scientific endorsement; and
- an explicit legacy comparison route remains temporarily available pending a
  separate review decision.

Phase 2 is a future Registry-first public-interface transition. It must not be
treated as current behavior or implemented as incidental maintenance work.

## Reading routes

This directory is being built in small reviewable slices.

| Task | Read next | Current status |
| --- | --- | --- |
| Understand the refactor and current data flow | `rr-crf-architecture.md` | Available |
| Fix or extend RR/CRF/EPI behavior safely | `rr-crf-maintenance-runbook.md` | Available |
| Review unresolved HIA methodology | `doc/hia-methodology-validation-open-questions.md` | Available |
| Use CRF Registry selections | `doc/crf-registry-selection.md` | Available |
| Inspect detailed historical planning evidence | `.github/prompts/dev/rr_crf_redesign/` | Private supporting history; read only when needed |

Use the architecture guide to locate the responsible layer and the maintenance
runbook to choose the required checks and review boundary.

## Current source map

| Concern | Primary implementation or evidence |
| --- | --- |
| Registry definitions and validation | `R/crf_registry.R`, `tests/testthat/test_crf_registry.R` |
| Scientific references | Registry reference records and compiler source metadata |
| CRF compilation | `R/compile_crfs_*.R`, `R/compile_crfs_utils.R` |
| Preset and Set selection | `R/crf_select.R`, `tests/testthat/test_crf_select.R` |
| Double-counting protection | `R/crf_double_counting.R`, `tests/testthat/test_crf_double_counting.R` |
| Legacy-call translation and routing | `R/hia_compatibility_adapter.R`, compatibility tests under `tests/testthat/` |
| Historical reproduction boundary | `R/historical_crf_registry.R`, historical Registry and migration tests |
| EPI readiness | `R/crf_epi_validation.R`, `R/generate_epi_helpers.R`, `tests/testthat/test_crf_epi_validation.R` |
| Cross-version fixed results | `tests/testthat/test_results_immutability.R` and fingerprint helpers/fixtures |
| Continuous validation | `.github/workflows/tests.yaml` |

## Safety boundaries

A maintainer or AI agent may diagnose and propose a routine software fix when
the expected behavior is already established. Stop for human review before:

- accepting a changed fixed result or adding an authorised difference;
- changing Registry, RR, CRF, or EPI scientific content;
- changing a data source, disease definition, counterfactual, age rule,
  uncertainty interpretation, or overlap rule;
- changing a public interface or the top-level compute flow; or
- presenting a historical definition as a recommended scientific default.

Urgency changes priority, not evidence requirements. A report or installation
failure may require immediate software work, but it does not authorise an
unreviewed methodology change.

## Document maintenance

- Keep this directory small and task-oriented.
- Record the last reviewed date and verified commit in each plan.
- Update the relevant plan in the same change that alters architecture,
  maintenance procedures, or validation commands.
- Link to machine-readable inventories and tests instead of duplicating large
  tables that can drift.
- Keep temporary branch names, personal working notes, meeting transcripts,
  and completed implementation diaries in private planning history rather than
  this shared entry point.
