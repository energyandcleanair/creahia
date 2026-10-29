# CREAHIA

`creahia` is an R package dedicated to health impact assessment (HIA) of air pollution. It is developed and maintained by the [Centre for Research on Energy and Clean Air (CREA)](https://www.energyandcleanair.org).

## Installing creahia
```r
install.packages("remotes")
remotes::install_github("energyandcleanair/creahia")
```

## Getting started
- Read the HIA workflow overview with diagrams: `doc/hia_overview.md`
- Uncertainty details: `doc/paf_uncertainty.md` and `doc/paf_epi_uncertainty.md`
- Example (Philippines): `examples/HIA_GEMM_Philippines_clean.R`

## Choosing health impact functions
Every function an HIA can use, both the PM2.5 RR curves (GEMM, Fusion, GBD) and the log-linear CRFs (asthma, preterm birth, NO2 mortality, ...), is a row in one Registry, with its literature reference. You pick the functions for an HIA with `crfs_set()`, by combining presets and then adding, removing or replacing rows.

Existing workflows using `rr_sources` and `crfs_version` keep working unchanged.

### Presets
```r
available_crf_presets()
describe_crf_preset("crea_others_v1")
```

| Preset | Contents |
|---|---|
| `gemm` | GEMM PM2.5 curves: NCD.LRI, LRI, COPD, IHD, LC, Stroke, LRI.child |
| `fusion` | Fusion PM2.5 curves: same as GEMM, plus Diabetes |
| `gbd2019` | GBD 2019 PM2.5 curves: LRI, COPD, IHD, LC, Stroke, Diabetes, LRI.child |
| `crea_others_v1` | CREA's other functions: asthma (NO2, PM2.5, O3), preterm birth, low birth weight, work absences, O3 COPD, NO2 and SO2 mortality |

The PM2.5 presets cover deaths, YLLs and YLDs. Combine one of them with `crea_others_v1`. When CREA updates its other functions, they will be released as `crea_others_v2`, and `crea_others_v1` stays available to reproduce earlier results.

### Example: GEMM curves with CREA's other functions
```r
crfs <- crfs_set(presets = c("gemm", "crea_others_v1"))
crfs   # one row per function: pollutant, cause, outcome, crf_id, reference_id

hia <- wrappers.compute_hia_two_images(
  perturbation_rasters = perturbation_rasters,
  baseline_rasters = baseline_rasters,
  regions = regions,
  crfs = crfs,
  epi_version = "default",
  pop_year = 2025
)
```

The selection is the complete list of functions used: `rr_sources` and `crfs_version` are ignored and nothing else is added.

To use other curves, swap the preset, e.g. `crfs_set(presets = c("gbd2019", "crea_others_v1"))`.

### Adjusting a selection
Replace, remove or add individual rows:
```r
crfs <- crfs_set(
  presets = c("gemm", "crea_others_v1"),
  replace = list(                                   # Fusion instead of GEMM for IHD
    list(crf_id = "fusion_pm25_ihd_adult_deaths_v1"),
    list(crf_id = "fusion_pm25_ihd_adult_ylls_v1")
  ),
  remove = list(list(pollutant = "PM25", cause = "Absences", outcome = "Absences"))
)
```

`preview_crf_set()` takes the same arguments and shows what each row becomes (`selected`, `replaced`, `removed`, `added`) without building the selection. To find alternatives:
```r
search_crf_registry(pollutant = "PM25", cause = "IHD", outcome = "Deaths")
search_crf_registry(query = "asthma")
```

### References
Each row has a `reference_id`. To list the references behind a selection:
```r
crfs %>%
  distinct(reference_id) %>%
  left_join(load_crf_references(), by = "reference_id") %>%
  select(reference_id, author, year, title, doi)
```

`available_crf_references()` lists every reference in the Registry.

### Checking a selection
```r
validate_crf_selection(crfs)                                    # one function per pollutant, cause and outcome
validate_crf_selection(crfs_set(presets = c("gbd2019", "crea_others_v1")),
                       check_double_counting = TRUE)            # also rejects an aggregate selected with its components
describe_dc_group("ncdlri_mortality")                           # which causes NCD.LRI covers
validate_crfs_against_epi(crfs, epi = get_epi("default"),
                          species = c("pm25", "no2", "o3_8h", "so2"))  # baseline data missing for any function
```

`gemm` and `fusion` fail the strict check by design: they include NCD.LRI and its components, and the HIA flags the components as `double_counted` (see below). `compute_hia()` runs `validate_crfs_against_epi()` itself and warns about missing baseline data.

### Current limits
- GBD 2021 and GBD 2023 curves aren't presets yet: use `rr_sources = RR_GBD2021` or `RR_GBD2023` for now.
- `gemm` has no Diabetes curve. The default `rr_sources = c(RR_GEMM, RR_ORIGINAL)` takes Diabetes from the older IER curves, which aren't in the Registry; otherwise `crfs_set(presets = c("gemm", "crea_others_v1"))` gives the same results as the default.

To see which functions an `rr_sources` call uses, without running it:
```r
inspect_hia_configuration(rr_sources = RR_GBD2023, crfs_version = "default", epi_version = "gbd2023")$registry_set
```

### Double counting
When a selection contains an aggregate (e.g. `NCD.LRI`) and its components (IHD, Stroke, ...), all rows are calculated and the components are flagged `double_counted = TRUE`: sum only `!double_counted` rows. Two competing aggregates for the same pollutant and outcome, e.g. `AllCause` and `NCD.LRI`, stop with an error: choose one.

More details: `doc/phase1-hia-user-guide.md` and `doc/crf-registry-selection.md`.


## Contact us
For any question or suggestion, please [contact us](mailto:hubert@energyandcleanair.org).
