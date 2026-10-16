# Phase 1 Merge Protection Audit

## Decision this audit supports

This audit supports a discussion with Hubert about whether
`feat/unify_rr_and_crf` has enough engineering and numerical protection to be
merged into `master`. It does not select the installation vehicle, approve the
remaining HIA methodology questions, or authorise the merge itself.

The current recommendation is **high engineering confidence, conditional on
the new installed-package CI check passing after the branch is pushed**.

## Comparison scope

- Audit date: 2026-10-16
- Target branch: `master`
- Verified local and remote `master` commit:
  `315c06bed1673ce8b1178fd7e6ad18d92a9313b5`
- Feature baseline before this audit slice:
  `2c689313750e6aeb506abc8dbc02c1b70a583d6e`
- Feature-only commits at that point: 159
- Production R files changed or added relative to `master`: 27

The comparison uses `master...HEAD`. The repository does not currently use a
branch named `main` as its merge target.

## Protection evidence

| Risk | Protection | Current evidence |
|---|---|---|
| Registry definitions or profile membership drift | Schema, compiler, Registry, migration-coverage, and profile-fingerprint tests | 25 historical CRF definitions and all 55 active memberships across the default, C40, and Krewski-South Africa profiles are inventoried and protected |
| Legacy call behavior changes | Compatibility matrix, wrapper-routing tests, warning/error checks, and direct source-family contracts | 14 of 14 required call contracts are `registry_ready`; ordinary supported calls use Registry routing by default |
| Historical configuration drift | Nine-configuration manifest, fixed fingerprints, live legacy-versus-Registry comparisons, and population/EPI assertions | All nine historical RR/EPI/population mappings have Registry-route evidence |
| Both calculation paths drift together | Fixed numerical fingerprints in addition to live parity comparisons | Release sweep passed 1,165 expectations with 0 failures, 0 warnings, and 0 skips; no fixed snapshot changed |
| Cross-version changes are mistaken for new authorised differences | Explicit historical allow-list and current-route zero-difference rule | 3,984 historical missing-result cases and 608 historical numerical changes were authorised; 0 unauthorised cases were found |
| Source checkout passes but installed package is incomplete | Formal installed-package check | Latest completed check: 0 errors, 9 warnings, and 7 notes; installed tests passed 3,082 expectations with 0 failures |
| EPI is missing a required disease-burden column or uncertainty estimate | Component-readiness validation and direct unit tests | New focused EPI/readiness run passed 80 expectations with 0 failures, warnings, or skips |
| Future CI only tests the source checkout | Separate development-test and installed-package steps in the same workflow | CI now runs `devtools::test()` and then fails if `devtools::check()` reports an error |

The most important distinction is between live parity and fixed fingerprints.
Live parity proves that the old and Registry routes agree today. Fixed
fingerprints provide a second alarm if both routes change in the same way.

## Gaps found and closed in this audit

The audit found three concrete protection gaps:

1. CI ran development tests but did not build and test the installed package.
   This would not have caught the earlier build-ignore problem that omitted an
   R source file from the installed package. An error-gated installed-package
   check is now part of CI.
2. The EPI compatibility test file was empty even though component readiness
   depends on its helpers. Direct tests now protect metadata exclusion, the
   required `estimate` column, all three uncertainty labels, missing values,
   and the complete-data case.
3. Two different internal functions had the same
   `validate_crfs_against_epi()` name. The later legacy function silently won,
   leaving the other implementation dead. The unused duplicate was removed;
   the active legacy warning-and-filter behavior and the public compute flow
   are unchanged.

No HIA formula, Registry row, fixed fingerprint, public function signature, or
top-level `compute_hia()` -> `compute_hia_paf()` -> `compute_hia_impacts()` flow
changed in this audit.

## Evidence boundaries

The following items prevent an unconditional “everything is complete” claim:

- The new CI step has been validated structurally and locally, but its first
  GitHub run cannot be observed until this commit is pushed.
- The last successful full development run before this audit had 3,092 passed
  expectations, 0 failures, 18 expected warnings, and 3 skips. A repeat inside
  the current restricted environment could not be used as release evidence
  because parallel tests could not open a local socket and plot tests could
  not download the configured font. The focused tests covering this code
  change passed.
- The 9 package warnings and 7 notes remain maintenance debt. They are not
  RR/CRF numerical failures, but they should not be described as a clean
  `R CMD check`.
- No line-coverage percentage is claimed. Protection is based on named
  contracts, component inventories, parity checks, fixed fingerprints, and
  installed-package validation.
- Q01-Q12 remain methodology-review questions. Numerical reproduction does
  not approve those scientific choices.
- The explicit `creahia.hia_compatibility_routing = FALSE` comparison route is
  retained pending discussion with Hubert.
- The installation vehicle and rollback procedure remain open until the team
  chooses between an isolated trial and a direct shared-package update.
- The Hubert runbook and root `AGENTS.md` work remain deliberately deferred for
  the detailed discussion requested after the other evidence is complete.

## Suggested statement to Hubert

> I have high confidence that the branch is protected against the RR/CRF
> regressions we identified: all 14 required call contracts and all nine
> historical configurations have parity and fixed-fingerprint evidence, the
> latest complete development and installed-package checks finished without
> failures or errors, and CI will now repeat both source and installed-package
> validation. I would make merge approval conditional on the first green CI
> run of the new installed-package check. This confidence is about engineering
> compatibility; the installation method, rollback plan, remaining methodology
> questions, and removal of the explicit legacy comparison switch still need
> separate decisions with Hubert.
