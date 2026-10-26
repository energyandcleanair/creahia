# RR/CRF Architecture

- Last reviewed: 2026-10-19

## Purpose

This document explains the RR/CRF architecture that is implemented now. It is
for maintainers who need to locate a problem, understand which layer owns it,
and avoid turning a software fix into an unreviewed HIA methodology change.

The package currently uses the compatibility-preserving Phase 1 architecture.
Phase 2 is described only as a future boundary at the end of this document.

## HIA meaning before code structure

An HIA calculation combines four choices that must remain independent:

1. exposure and counterfactual concentrations;
2. exposure-response relationships, represented by CRFs or RR curves;
3. baseline epidemiological data (EPI); and
4. population data and, when requested, population-year scaling.

The exposure-response relationship produces a population attributable fraction
(PAF). The package then multiplies the PAF by the matching EPI burden to obtain
attributable impacts. Reproducing an old result proves that the software can
repeat that calculation; it does not prove that the old scientific choices are
recommended today.

## Current flow at a glance

```text
legacy-shaped compute_hia() call
        |
        v
normalise old and current RR selectors
        |
        v
compatibility boundary
  - identify CRF, RR, EPI and population intent separately
  - check whether every required component is ready
  - build one canonical HIA specification
        |
        v
route plan
  - ready supported configuration -> build an internal Registry Set
  - unsupported or not-ready configuration -> preserve the existing route
        |
        v
compute_hia_paf()
  - Registry Set -> Registry CRF calculation
  - remaining legacy input -> established legacy calculation
        |
        v
compute_hia_impacts()
        |
        v
optional population scaling and final result
```

An explicit `crfs_set()` enters the Registry calculation directly. It does not
need the legacy compatibility translation.

The public top-level compute flow remains:

```text
compute_hia() -> compute_hia_paf() -> compute_hia_impacts()
```

## Responsibilities by layer

### 1. Public HIA boundary

Primary file: `R/compute_hia.R`

`compute_hia()` receives concentration maps, regions, CRF choices, RR choices,
EPI, and population-year intent. It records whether key inputs were explicitly
supplied, translates deprecated population arguments, and preserves the public
calculation sequence. It restricts concentration maps to the requested
`scenarios` before calculation, so RR, CRF and impact stages use the same
selection. Standalone `compute_hia_paf()` calls apply the same selection at
their entry boundary. Unknown scenario names are rejected.

The function does not decide that a historical method is scientifically
approved. It asks the compatibility layer whether the supplied configuration
can be represented safely by the Registry-driven path.

### 2. Compatibility boundary and route planning

Primary files:

- `R/hia_compatibility_adapter.R`
- `R/hia_component_readiness.R`
- `R/hia_specification.R`

The compatibility boundary has four jobs:

1. translate `calc_causes` and `gbd_causes`, when present, into the existing RR
   source representation;
2. resolve CRF, RR and EPI requirements as separate components;
3. compare those components with the readiness evidence; and
4. create a route plan without changing the HIA formula.

The canonical HIA specification records the resolved components, EPI version,
population-year intent, ignored RR pairs, blockers, and the input contract used
as evidence. It is an internal representation, not a new public API.

A component is ready for routing only when its required data exist, its
historical result has been reproduced, and its visibility boundary is valid.
Scientific review status is recorded separately. A historical component may be
ready for reproduction while still being `not_reviewed` scientifically.

### 3. Registry and References

Primary files and data:

- `R/crf_registry.R`
- `inst/extdata/crf/registry.csv`
- `inst/extdata/crf/references.csv`
- `inst/extdata/crf/tabular/`

The Registry stores one structured row per CRF definition. A row identifies
the pollutant, cause, outcome, age range, functional form, parameters or data
path, counterfactual, units, and `reference_id`. Overlap decisions belong to the
calculation selection, not to individual CRF definitions.

References describe the scientific source. Registry validation requires every
`reference_id` to exist in the References table. Updating a Registry row and
updating its scientific source are related but distinct decisions.

Log-linear CRFs store their parameters in the Registry row. Tabular CRFs point
to a versioned table under `inst/extdata/crf/tabular/`. The compiler files
`R/compile_crfs_*.R` turn reviewed source data into these common forms.

### 4. Presets and Sets

Primary files and data:

- `R/crf_select.R`
- `inst/extdata/crf/presets/`
- `doc/crf-registry-selection.md`

A Preset is a named, reusable selection of Registry CRFs. A Set is the explicit
resolved collection used by a calculation. `crfs_set()` can combine presets
with reviewed additions, removals, or replacements and validates the Registry
shape of the result. It does not reject aggregate/component overlap by default.

Public discovery filters out internal fixtures and internal presets. Internal
presets are never defaults; an explicit name can load one for development, but
it is not reviewed or a recommended method.
Historical-compatibility rows live in separate internal storage and therefore
do not enter public discovery. Separately, the public Registry currently has
two `legacy_` rows migrated from the old `CRFs.csv` table. They can be found and
selected, but are not methodologically approved or recommended defaults.
`legacy_no2_ncdlri_deaths_v1` cites the temporary `legacy_default_crfs`
provenance record; `legacy_no2_asthma_1to18_incidence_v1` cites
`khreis_2017_childhood_asthma`. A test
fixture is not a user option, and a historical profile is not a recommended
default merely because the package can reproduce it.

### 5. Historical reproduction storage

Primary files and data:

- `R/historical_crf_registry.R`
- `R/historical_rr_manifest.R`
- `inst/extdata/crf/compatibility/historical_registry.csv`
- `inst/extdata/crf/compatibility/historical_memberships.csv`
- `inst/extdata/rr/compatibility/historical_registry.csv`
- `inst/extdata/rr/compatibility/historical_manifest.csv`

Historical storage exists so supported old configurations can be represented
in Registry shape without advertising unresolved definitions to users.

Together, historical storage and the component-readiness manifest keep
separate fields for:

- reproduction status;
- routing status;
- scientific review status;
- internal visibility; and
- provenance or blocking reasons.

Only parity-verified components can be assembled for compatibility routing.
They remain internal unless separately reviewed and promoted through the public
Registry process.

### 6. Registry Set resolution

Primary file: `R/hia_diagnostic_registry_set.R`

When all components are ready, the resolver combines historical CRF and RR
components into one Registry-shaped Set and validates the complete selection.
It does not store precomputed overlap flags on the Set.

The name of this internal file reflects its development history. In the current
Phase 1 flow, the same resolver supplies the active compatibility route. Do not
rename or reorganise it as incidental maintenance; first protect callers and
tests.

### 7. PAF calculation

Primary files:

- `R/compute_hia_paf.R`
- `R/crf_apply.R`

`compute_hia_paf()` selects the calculation path. A `crfs_set()` forces the
Registry path. The Registry calculation loops over selected CRFs, scenarios,
and regions, then applies each CRF through the common `apply_crf()` interface.

On the Registry compatibility route, historical RR curves have already been
represented as Registry-shaped CRFs. The separate legacy `rr_sources` input is
therefore emptied to prevent the same burden from being calculated twice.

Compatibility-generated RR CRFs are identified by the internal `rr_crf_ids`
provenance marker. Their complete cases use only that pollutant's baseline,
scenario, and population columns, matching the legacy RR route. Errors are
isolated per scenario and region: a `Failed for region` warning naming the
failed `crf_id` and pollutant/cause/outcome is emitted and all RR results for
that region are omitted. Log-linear CRFs retain their existing whole-table
missingness and fail-fast behavior. Tabular CRFs in an explicit `crfs_set()`
use the same per-pollutant complete-case rule as compatibility RR CRFs, so the
same curve gives the same PAF through either entry point; log-linear CRFs in an
explicit Set keep the whole-table rule. All other Registry CRFs, including
every CRF of an explicit `crfs_set()`, are fail-fast by maintainer decision so
that a failing region never silently lowers totals: the calculation still
visits every scenario, region and CRF, collects each failure, and then stops
once with an error listing every failing region, `crf_id`, slot and message
(`.format_registry_region_errors()`). No partial results are returned. Users
who need the legacy skip behaviour use the legacy-shaped call or exclude the
listed regions. An opt-in skip mode is a Phase 2 interface decision. An
inspected compatibility Registry Set reused directly as `crfs` is
treated as a continuation of that legacy configuration and retains its
diagnostic provenance, so it reproduces the legacy-shaped call's results.

### 8. Impact calculation

Primary files:

- `R/compute_hia_impacts.R`
- `R/multiply_paf_epi.R`

`compute_hia_impacts()` matches PAF rows with regional EPI data, multiplies the
fractions by baseline burden, adds overlap and age information, and returns the
final long-form impacts. Population-year scaling happens afterward in
`compute_hia()` when requested.

## Four choices that must not be collapsed

| Choice | What it controls | What it must not silently control |
| --- | --- | --- |
| CRF profile or Set | Which CRF definitions are selected | RR source, EPI version, population year |
| RR source selection | Which RR families and causes are used | CRF profile or EPI data |
| EPI version | Which baseline burden data are used | Exposure-response source or population year |
| Population year | Whether final impacts are scaled to another year | CRF, RR, or EPI selection |

This separation is essential for reproducing a historical analysis. Two runs
that share a CRF label but use different RR, EPI, or population choices are not
the same configuration.

## Double-counting boundary

Primary files and data:

- `R/crf_double_counting.R`
- `inst/extdata/crf/dc_groups.csv`

Some aggregate causes overlap with their component causes. For example, an
aggregate burden must not be added to the component burdens that it already
contains. Ordinary Set construction allows both alternatives to be retained
for comparison. A maintainer must explicitly request strict overlap validation
with `validate_crf_selection(..., check_double_counting = TRUE)` before treating
a selection as non-overlapping or summable.

At the start of `compute_hia()`, after routing and final selection and before
PAF calculation, one resolver builds an overlap plan from the selected CRF
and RR cause slots. It uses defined aggregate/component breakdowns within each pollutant/outcome.
Competing whole-burden aggregates (AllCause, NCD.LRI, NCD) stop setup and require
one aggregate to be selected. AllCause with alternative disease-specific
estimates retains both, leaving the reporting choice to the caller; filtering
only `!double_counted` is insufficient for that selection. Strict overlap
validation still rejects those alternatives together. The coverage table uses
`alternative_component` for these alternatives and `component` for automatic
breakdown exclusions. Groups containing multiple aggregate rows and no
components identify competing aggregates. The plan stays local to this calculation and is passed
directly to `compute_hia_impacts()` through its internal `.double_counting`
argument. Every calculation rebuilds it, including calculations using filtered
or extended inspected Sets. Registry definitions and compilers no longer store
`double_counted`; archived legacy input tables retain their original flags for
provenance only and those flags do not control calculation.

`compute_hia_impacts()` attaches the plan's flags without choosing again. It
verifies that excluded components have a non-missing covering ensemble in the
same region, scenario and estimate. Missing coverage is an error, not a switch
to component estimates. For standalone calls with precomputed PAFs, the same
resolver runs once at the start of impact calculation using the available PAF
slots. Precomputed PAFs cannot identify selected causes that were never
calculated/exported. The standalone `add_double_counted()` helper also uses
this resolver.

Cause coverage lives in `dc_groups.csv`, including `TotCV` (IHD, Stroke,
OthCV), `TotResp` (COPD, OthResp), and enclosing NCD.LRI/AllCause relationships.
`TotResp` covers chronic respiratory diseases; LRI is separate. The historical
`CV` rule retains its existing IHD/Stroke membership rather than being silently
reinterpreted as TotCV.

Passing numerical parity does not by itself approve a new aggregate mapping.
Changing an overlap group or interpreting a new aggregate requires methodology
review.

## Route behavior and limits

| Input situation | Current behavior |
| --- | --- |
| Explicit `crfs_set()` | Use the complete Registry Set directly; omitted `rr_sources` adds no legacy RR, explicit non-overlapping RR may be combined, and overlapping slots are rejected before PAF calculation |
| Supported legacy-shaped configuration with all components ready | Resolve a canonical specification and use an internal Registry Set |
| Explicit CRF data frame | Leave unchanged; it does not enter the current compatibility-adapter slice |
| CRF profile absent from compatibility storage | Leave unchanged |
| Required component not ready | Preserve the legacy route and report blockers through inspection/diagnostics |
| `creahia.hia_compatibility_routing = FALSE` | Keep the legacy comparison route temporarily |

`inspect_hia_configuration()` in `R/hia_configuration_inspection.R` exposes the
planned route, resolved components, references, blockers, and population intent
without running the HIA. Use it to understand a configuration before debugging
the numerical calculation. Its `registry_set` can be reused as a complete Set;
RR-derived rows retain compatibility missingness and regional error isolation.

Shadow and dual-run observers are diagnostic tools. When a diagnostic observer
is enabled and routing has not been explicitly configured, routing defaults to
off so the established path can be observed. Active routing and dual-run mode
cannot be enabled in the same call. These tools compare or observe routes; they
do not define scientific approval.

## Protection around the architecture

| Risk | Main protection |
| --- | --- |
| Invalid Registry shape or references | Registry, reference, compiler, and selection validation tests |
| Wrong legacy-to-Registry mapping | Compatibility matrix and component-readiness tests |
| Old and new routes disagree | Live parity and dual-run tests |
| Both routes change together | Fixed PAF and final-impact fingerprints |
| Historical releases legitimately differ | Narrow rules in `tests/testthat/test_results_immutability.R` |
| Source checkout works but installed package is incomplete | Installed-package check in `.github/workflows/tests.yaml` |
| Aggregate and component burden are mixed | Explicit strict double-counting validation and Set tests; ordinary Set construction is not a summability guarantee |

Never update a fixed result or authorised-difference rule until the cause and
review owner are known. The maintenance runbook will provide the task-specific
test commands and response steps.

## Current Phase 1 boundary

Phase 1 keeps established public calls working while routing supported ordinary
configurations through the Registry-driven calculation. It protects historical
behavior with readiness checks, parity comparisons, and fixed results.

Phase 1 does not mean that:

- every historical definition is scientifically recommended;
- every public argument has been redesigned;
- the explicit legacy comparison option has been removed; or
- the package has completed a Registry-first breaking change.

## Future Phase 2 boundary

Phase 2 is the future Registry-first public-interface transition. Its exact
public API, formal defaults, migration policy, and removal of legacy selectors
must be decided separately. A maintenance task must not implement Phase 2 by
quietly deleting compatibility behavior or promoting historical definitions.

## Architecture update rule

Update this document when a change alters:

- ownership between Registry, References, Presets, Sets, or the compatibility
  adapter;
- the canonical specification or route decision;
- the top-level PAF or impact flow;
- historical/public visibility boundaries;
- double-counting behavior; or
- the Phase 1 and Phase 2 boundary.

Update the verified commit only after comparing this document with the actual
implementation and tests.
