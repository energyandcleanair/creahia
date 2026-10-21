# RR/CRF Maintenance Runbook

- Last reviewed: 2026-10-19

## Purpose

This runbook is for maintainers and AI agents handling RR/CRF-related failures,
data updates, or small extensions. It explains what can be changed as routine
software maintenance, what needs human review, and which evidence is required
before a change is considered complete.

Read the root `AGENTS.md` and `plans/rr-crf-architecture.md` first. Run commands
from the repository root.

## Non-negotiable rules

1. Protect unrelated work. Check the branch and working tree before editing.
2. Reproduce the smallest failing behavior before proposing a fix.
3. Keep CRF selection, RR source, EPI version, and population year separate.
4. Do not change scientific content as a side effect of fixing software.
5. Never update a fingerprint, snapshot, tolerance, or authorised-difference
   rule only to make a test pass.
6. Treat historical reproduction and scientific approval as separate claims.
7. Preserve `compute_hia()` -> `compute_hia_paf()` ->
   `compute_hia_impacts()` unless a separately approved architecture change
   explicitly requires otherwise.
8. Start with focused tests. Expand validation in proportion to the risk.

## Start every maintenance task

### 1. Record the repository state

```sh
git status --short --branch
git log -1 --oneline
```

Do not clean, reset, restore, or reformat unrelated files. Existing changes may
belong to another maintainer.

### 2. Record the failure

Capture:

- the complete call or workflow that failed;
- expected and actual behavior;
- warnings and error text;
- CRF profile or Set;
- RR source selection;
- EPI version;
- population year;
- package version and commit;
- whether the failure occurs from the source checkout, installed package, or
  both; and
- the smallest data needed to reproduce it.

Do not begin by editing code. First determine which input combination and which
calculation stage are affected.

### 3. Inspect the resolved configuration

The default configuration can be inspected without running an HIA:

```sh
Rscript -e 'devtools::load_all(); inspection <- inspect_hia_configuration(); print(inspection); print(inspection$blocking_components); print(inspection$references)'
```

For a reported workflow, call `inspect_hia_configuration()` with the same
`rr_sources` or legacy cause selectors, `epi_version`, `crfs_version`, and
population arguments as the failing HIA.

The inspection result answers:

- whether the call is eligible for compatibility routing;
- whether it plans the Registry, legacy, or unchanged route;
- which CRF, RR, and EPI components were resolved;
- which components block routing; and
- which scientific references are attached to the resolved Set.

### 4. Classify the task before changing files

| Task type | Examples | May routine maintenance proceed? |
| --- | --- | --- |
| Established software behavior is broken | error, missing installed file, wrong dispatch, wrapper shape changed | Yes, with focused regression evidence |
| Environment or input problem | missing key, font, GIS data, malformed caller input | Diagnose first; do not change package science to hide it |
| Scientific data content changes | RR values, CRF coefficients, EPI values, source metadata | No; obtain data/method review before acceptance |
| Method assumption changes | cause, outcome, age, counterfactual, uncertainty, overlap | No; method reviewer decides |
| Public API or compute-flow changes | argument removal, new default, changed return type, reordered compute flow | No; agree scope and migration plan first |
| Fixed result changes | PAF or final-impact fingerprint differs | Stop; explain the difference before editing the fixture |

Urgency changes priority, not the review boundary. A report-blocking error may
be fixed quickly when expected behavior is already known. It does not authorise
a new scientific choice.

## Validation ladder

Use the lowest sufficient level first, then move upward when the change affects
shared behavior.

### Level 1: focused tests

Choose the filter for the affected layer from the task sections below. General
form:

```sh
Rscript -e 'devtools::test(filter = "FILTER", stop_on_failure = TRUE)'
```

Record the number of passed expectations, failures, warnings, and skips.

### Level 2: RR/CRF protection sweep

Use when routing, numerical behavior, Registry content, historical mappings,
or shared HIA computation may be affected:

```sh
Rscript -e 'devtools::test(filter = "fingerprint|parity|compatibility|contract", stop_on_failure = TRUE)'
```

This combines live old-versus-new comparisons with fixed expected results. It
does not replace the full package suite.

### Level 3: full development suite with known side effects

Use after changing shared computation, routing, selection, packaged data, or
several test families:

```sh
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

This command includes `test_results_immutability.R`. That test can install old
package releases, regenerate current-version fingerprint files, and create a
missing historical fingerprint. Before running it, require a clean working
tree, confirm that network/package installation is acceptable, and compare
`git status` before and after. If those side effects are not appropriate for
the current task, stop after the focused protection levels and schedule the
full suite as an explicit release-validation step. Do not describe the reduced
run as the full suite.

### Level 4: installed-package check

Use when changing `DESCRIPTION`, `.Rbuildignore`, package data, file lookup,
dependencies, generated documentation, or anything that could behave
differently after installation:

```sh
Rscript -e 'result <- devtools::check(document = FALSE, manual = FALSE, error_on = "never"); print(result); if (length(result$errors) > 0) quit(status = 1)'
```

Zero errors is required. Warnings and notes must be compared with the previous
known result and explained; they must not be hidden merely to make the check
look clean.

### Level 5: cross-version comparison

Use for release-level work or a deliberate result change:

```sh
Rscript -e 'devtools::test(filter = "results_immutability", stop_on_failure = TRUE)'
```

This test can install older package releases, regenerate current-version
fingerprints, and create a missing historical fingerprint. It may need network
access and local package dependencies. Record `git status` before and after it,
inspect every generated file, and do not retain an unexplained change.

## Task A: urgent report or workflow failure

Use this path when an established report cannot run or a supported call now
fails.

### Diagnose

1. Reproduce the exact error with the smallest available input.
2. Run `inspect_hia_configuration()` with the reported selectors.
3. Determine whether failure occurs before PAF calculation, during PAF
   calculation, during EPI multiplication, during population scaling, or in a
   wrapper/output step.
4. Run the same focused call from the source checkout and, when relevant, from
   the installed package.
5. Check whether the failure is caused by an unavailable external input rather
   than package behavior.

### Likely ownership

| Failure point | Start with |
| --- | --- |
| Call translates to the wrong configuration | `R/hia_compatibility_adapter.R`, `R/hia_component_readiness.R` |
| Registry Set cannot be assembled | `R/hia_diagnostic_registry_set.R`, historical manifests and registries |
| CRF produces the wrong PAF | `R/compute_hia_paf.R`, `R/crf_apply.R` |
| PAF does not match EPI | `R/crf_epi_validation.R`, `R/multiply_paf_epi.R` |
| Final impacts or output fields are wrong | `R/compute_hia_impacts.R`, wrapper code |
| Population scaling is wrong | population helpers and the supplied `pop_year` |

### Minimum validation

Run the test for the directly affected file, the matching wrapper or contract
test, and the RR/CRF protection sweep if numbers or routing can change.

Do not replace a missing method/data choice with a guessed default merely to
restore a report.

## Task B: package installation or installed-only failure

Source-checkout success does not prove that the installed package exposes the
same behavior and packaged data. Installed R packages normally do not retain
the individual files under `R/`, so the absence of an installed
`R/some_function.R` file is not by itself a defect.

### Diagnose

1. For R code, confirm that the required function is available through the
   installed namespace and test its behavior. Do not test for its original
   source filename after installation.
2. For packaged data or other artifacts, check `DESCRIPTION`, `.Rbuildignore`,
   and the relevant path under `inst/extdata/`.
3. Confirm that `get_hia_path()` or `system.file()` can resolve the packaged
   artifact after installation.
4. Check whether a test relies on repository-only folders such as `examples/`,
   private plans, or local raw data.
5. Check direct dependency declarations rather than relying on a package being
   attached indirectly.
6. Run the installed-package check above.

### Stop conditions

- Do not move scientific data merely to satisfy a build layout without
  confirming the expected installed path.
- Do not weaken `.Rbuildignore` broadly; anchor exclusions narrowly.
- Do not skip an installed-package test solely because it exposed a real
  packaging defect.

### Completion evidence

- the source-checkout behavior passes;
- the installed namespace exposes the required R behavior;
- the installed package contains and resolves each required packaged artifact;
- installed-package tests have no failures; and
- `devtools::check()` has no errors.

## Task C: add or update a CRF

A CRF change can alter attributable fractions directly. Treat coefficients,
tables, age ranges, counterfactuals, outcomes, and source references as
scientific content.

### Decide the destination first

| Destination | Meaning |
| --- | --- |
| Public Registry | Reviewed definition intended for user discovery or explicit selection |
| Historical compatibility storage | Exact old definition needed only to reproduce a supported historical workflow |
| Test fixture | Synthetic data used only to test behavior |

Do not promote a historical or fixture row into the public Registry because it
already happens to reproduce a result.

### Files to inspect

- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/references.csv`
- `inst/extdata/crf/tabular/`
- `R/compile_crfs_utils.R`
- the matching `R/compile_crfs_*.R`
- `R/crf_registry.R`
- `R/crf_apply.R`
- the relevant preset under `inst/extdata/crf/presets/`

Historical-only CRFs instead use:

- `inst/extdata/crf/compatibility/historical_registry.csv`
- `inst/extdata/crf/compatibility/historical_memberships.csv`
- `R/historical_crf_registry.R`

### Required sequence

1. Record the source, citation, units, pollutant, cause, outcome, age contract,
   functional form, counterfactual, and overlap meaning.
2. Obtain scientific review before accepting new or changed values.
3. Add or update a compiler/validator before editing generated output.
4. Generate a candidate with Registry and Reference updates disabled when the
   compiler supports those switches.
5. Compare the candidate with its source and expected age/exposure coverage.
6. Add the Registry and Reference changes only after review.
7. Add selection or preset membership separately; a valid Registry row does
   not automatically belong in a preset.
8. Add PAF parity and final-impact coverage when replacing an established path.

Existing public compilers default `update_registry = FALSE` and
`update_references = FALSE`, but they can still write tabular artifacts. Do not
use `overwrite = TRUE` until the candidate source and expected diff have been
reviewed.

### Minimum tests

Registry and application behavior:

```sh
Rscript -e 'devtools::test(filter = "crf_registry|crf_apply", stop_on_failure = TRUE)'
```

Use the matching compiler family as well:

```sh
Rscript -e 'devtools::test(filter = "crf_gemm_migration|crf_gbd_migration|crf_fusion_migration", stop_on_failure = TRUE)'
```

If the CRF enters a Preset, Set, historical configuration, or existing output,
also run the selection, double-counting, parity, and fixed-result checks.

## Task D: add or update an RR source

RR changes affect tabular exposure-response curves, their age contracts, and
the cause/outcome pairs calculated from them.

### Files to inspect

- `inst/extdata/rr/processed/rr_*.csv`
- `R/get_rr.R`
- `R/generate_rr_*.R`
- `R/generate_rrs.R`
- `R/compile_crfs_gemm.R`, `R/compile_crfs_gbd.R`, or
  `R/compile_crfs_fusion.R`
- `R/historical_rr_manifest.R`
- `R/compile_historical_*_rr.R`
- `inst/extdata/rr/compatibility/`

### Required sequence

1. Record the original source and generation method.
2. Confirm cause, outcome, age labels, exposure grid, low/central/high values,
   and source-family identity.
3. Decide whether the curve is reviewed public content or historical-only
   reproduction data.
4. Regenerate through the appropriate compiler rather than hand-editing copied
   curves.
5. Check for missing ages, duplicate exposure rows, conflicting duplicates,
   and unexpected extrapolation.
6. Update the historical manifest only when its coverage and readiness claims
   are supported by parity evidence.
7. Test both PAF and final impacts for an established configuration.

### Minimum tests

```sh
Rscript -e 'devtools::test(filter = "rr_sources|age_coverage|historical_rr_requirements", stop_on_failure = TRUE)'
```

Then run the matching source-family and historical parity tests. If an existing
configuration changes, stop at any fixed-result difference until it is
explained and reviewed.

## Task E: update EPI data

EPI determines the baseline burden multiplied by PAF. A valid CRF result can
still produce a wrong final impact if the EPI version, metric key, estimate, or
location mapping is wrong.

### Files to inspect

- `R/generate_epi.R`
- `R/generate_epi_helpers.R`
- `R/hia_data.R`
- `R/crf_epi_validation.R`
- `R/multiply_paf_epi.R`
- `inst/extdata/epi/processed/`
- local source files under `data/epi_update/`, when available

### Required sequence

1. Record the upstream release, download date, geography, measure, unit, and
   transformation steps.
2. Keep the EPI version independent of the RR source and CRF profile.
3. Inspect the generated or packaged data before runtime imputation. Confirm
   where `low`, `central`, or `high` values are wholly or partly missing for
   each required metric key.
4. Check cause and outcome names after `fix_epi_cols()` and other recoding.
5. Check location IDs, population fields, missing locations, and duplicate
   estimates.
6. Compare changed final impacts with unchanged PAFs to isolate the EPI effect.
7. Obtain data/method review before replacing packaged values or changing the
   default EPI version.

Current behavior matters when interpreting these checks: `get_epi()` calls
`adddefos()` before component-readiness validation. That helper can fill
numeric missing values from regional/income-group and then estimate-wide
medians. The readiness check proves that every required metric/estimate exists
somewhere after that processing; it does not prove that every location had an
original non-missing value. Inspect pre-imputation data for partial gaps, and
do not broaden or approve median imputation for a new dataset without data or
method review.

### Minimum tests

```sh
Rscript -e 'devtools::test(filter = "crf_epi_validation|epidemiological|epi_subnational|compute_hia_impacts", stop_on_failure = TRUE)'
```

Also run historical configuration fingerprints when the changed EPI version is
used by a protected workflow.

## Task F: change a Preset, Set, or double-counting rule

Presets are reusable selections. Sets are resolved analysis configurations.
Changing membership can alter which burdens are present even when every CRF is
individually valid.

### Files to inspect

- `R/crf_select.R`
- `inst/extdata/crf/presets/`
- `R/crf_double_counting.R`
- `inst/extdata/crf/dc_groups.csv`
- `doc/crf-registry-selection.md`

### Required sequence

1. State the intended analysis choice in pollutant/cause/outcome terms.
2. Preview the selection before constructing the final Set.
3. Confirm every CRF is public and reviewed for the intended use.
4. Run strict overlap validation explicitly:
   `validate_crf_selection(crfs, check_double_counting = TRUE)`.
   Ordinary `crfs_set()` construction and default selection validation do not
   reject an aggregate/component conflict.
5. Keep aggregate and component alternatives separately selectable when they
   represent overlapping burden. They may coexist for comparison, but must not
   be summed or presented as non-overlapping.
6. Update user documentation when public discovery or examples change; internal presets must not appear in user discovery or become defaults. An explicit internal name is development-only, not reviewed, and not a recommended method.

### Minimum tests

```sh
Rscript -e 'devtools::test(filter = "crf_select|crf_double_counting|registry_smoke|crf_fixture_isolation", stop_on_failure = TRUE)'
```

Changing `dc_groups.csv` is a methodology change, not ordinary selection
maintenance. It requires evidence describing which burden overlaps and why.

## Task G: change compatibility routing or a wrapper

This is high-risk because a small dispatch change can affect many established
calls without changing their visible arguments.

### Files to inspect

- `R/compute_hia.R`
- `R/hia_compatibility_adapter.R`
- `R/hia_component_readiness.R`
- `R/hia_specification.R`
- `R/hia_diagnostic_registry_set.R`
- `R/hia_configuration_inspection.R`
- the affected wrapper file

### Required invariants

- route selection is based on resolved components, not a hard-coded contract
  name;
- CRF, RR, EPI, and population intent remain separate;
- a Registry route replaces its compute inputs atomically;
- explicit custom CRFs stay outside the current compatibility slice unless a
  reviewed rule is added;
- incomplete components do not masquerade as ready;
- wrapper arguments, warnings, errors, return type, and file outputs remain
  stable unless an approved breaking change says otherwise; and
- the explicit comparison route is not removed without its separate decision.

### Minimum tests

```sh
Rscript -e 'devtools::test(filter = "hia_compatibility|hia_component_readiness|hia_specification|hia_configuration_inspection|hia_readiness_shadow|legacy_compatibility_matrix|legacy_workflow_compatibility", stop_on_failure = TRUE)'
```

Also run affected wrapper tests and the RR/CRF protection sweep. If the change
affects installed behavior, run the full suite and installed-package check.

## Task H: investigate a numerical or fingerprint difference

Do not edit the expected file first.

### Investigation order

1. Re-run the single failing test without regenerating its fixture.
2. Check `git diff` for code, packaged data, test helpers, tolerances, and
   fixtures.
3. Confirm the exact CRF profile/Set, RR source, EPI version, population year,
   package version, and route.
4. Compare the earliest numerical layer available:
   - selected Registry rows;
   - applied CRF or RR curve;
   - PAF rows;
   - EPI rows;
   - final impacts; and
   - wrapper/output structure.
5. Decide whether the change is caused by code, scientific data, environment,
   ordering/metadata only, or an intended reviewed correction.
6. Check whether live parity still passes. If both routes moved together, the
   fixed fingerprint is the evidence that exposed the drift.
7. Ask for human review before accepting any numerical change.

Useful checks:

```sh
git status --short
git diff -- tests/testthat/fixtures inst/testdata/fingerprints
Rscript -e 'devtools::test(filter = "fingerprint|parity", stop_on_failure = TRUE)'
```

### Requirements for an accepted difference

Record all of the following:

- cause of the change;
- affected version and configuration;
- affected rows or values;
- expected scientific or software meaning;
- source or review evidence;
- decision owner; and
- the narrow test or authorised-difference rule that protects the decision.

Current-route Legacy and Registry equivalence has no general authorised
numerical difference. Cross-version exceptions in
`tests/testthat/test_results_immutability.R` must remain narrow and historical.

## Task I: change the public API or compute flow

Do not implement this as routine maintenance.

Before editing:

1. state the user problem and affected workflows;
2. identify whether this belongs to current Phase 1 maintenance or future
   Phase 2;
3. agree the compatibility and migration policy;
4. define old and new call examples and return contracts;
5. define numerical, warning/error, wrapper, and installation acceptance
   criteria; and
6. obtain explicit approval for the scope.

Removing old selectors, changing formal defaults, removing the explicit
comparison option, or changing the top-level compute sequence are separate
architecture decisions.

## Release and handover checks

Before recommending a merge or shared-package update:

1. confirm focused tests for every changed layer;
2. run the RR/CRF protection sweep when numerical or routing behavior is in
   scope;
3. run the full development suite;
4. run the installed-package check;
5. inspect warnings, notes, skips, and environment-only messages separately;
6. confirm CI repeats both development and installed-package validation;
7. record any unresolved methodology or data questions without presenting them
   as engineering failures; and
8. define the chosen installation and rollback steps before cutover.

Do not claim release readiness from a successful demonstration or focused test
alone.

## Maintenance completion report

Every completed task should report:

```text
Problem:
Classification: software / environment / data / methodology / API
Root cause:
Files changed:
Behavior preserved or intentionally changed:
Numerical effect:
Focused tests:
Broader tests and installed check:
Warnings or skips:
Human review obtained or still required:
Rollback or revert point:
Remaining limitations:
```

## Clean-context handover checks

A maintainer or AI agent using only `AGENTS.md` and `/plans` should reach these
conclusions:

1. **A fixed result changed:** do not update it; locate the first changed
   numerical layer and request review.
2. **A new CRF is requested:** establish source, review status, destination,
   compiler, Registry/Reference records, and parity evidence before selection.
3. **EPI lacks an uncertainty estimate:** if an entire required estimate is
   absent, treat the component as incomplete. If only some locations are
   missing, inspect the pre-imputation data and the existing `adddefos()`
   result; do not silently approve or broaden imputation in a maintenance fix.
4. **A report is urgently blocked:** fix established software behavior, but
   stop if recovery requires a new scientific choice.
5. **A Preset mixes aggregate and component burden:** run explicit strict
   overlap validation and expect it to fail. Do not assume Set construction
   blocks the combination, and do not make the results summable.
6. **Installed behavior or packaged data is missing:** test R behavior through
   the installed namespace and data through installed package paths. The
   absence of an individual installed `R/*.R` source file is not itself a
   defect.
7. **A request removes legacy arguments or the comparison route:** classify it
   as an approved Phase 2 or architecture decision, not routine cleanup.

If the documents do not lead clearly to these conclusions, update the handover
documentation before relying on it for unsupervised maintenance.
