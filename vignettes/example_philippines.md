CREAHIA: Philippines example
================

    #> Loading required package: sp

# Installation

``` r
library(remotes)
remotes::install_github("energyandcleanair/creahia")
```

``` r
library(creahia)
#> Warning in fun(libname, pkgname): rgeos: versions of GEOS runtime 3.9.1-CAPI-1.14.2
#> and GEOS at installation 3.8.1-CAPI-1.13.3differ
#> [1] "Initiating environment"
```

Configuration:

``` r
calpuff_dir <- "../examples/example_ph"
gis_dir <- "/Volumes/ext1/gis/" # Where to find baseline concentration etc. Contact-us to have access to the required data
```

# Create concentration fields

To perform an health impact assessment, `creahia` requires: -
`conc_scenario`: the concentration field of one or several species in
one or several scenarios for which we want to evaluate the health impact
- `conc_baseline`: the baseline concentration of the selected species.

We also define `conc_perturbation` such that `conc_scenario =
conc_baseline + conc_perturbation`.

## Read CALPUFF results

``` r
conc_perturbation <- creahia::get_conc_calpuff(calpuff_dir=calpuff_dir, utm_zone=51, utm_hem='N', map_res=1)

species <- unique(conc_perturbation$species)
scenarios <- unique(conc_perturbation$scenario)
grid_raster <- conc_perturbation$conc_perturbation[[1]] %>% raster::raster()
grid_raster
#> class      : RasterLayer 
#> dimensions : 1493, 863, 1288459  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 34.57735, 897.5773, 558.8777, 2051.878  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=51 +datum=WGS84 +units=km +no_defs
```

Plot pm25 perturbation:

``` r
conc_pm25 <- conc_perturbation[conc_perturbation$species=="pm25",]
rs <- raster::stack(conc_pm25$conc_perturbation)
names(rs) <- paste(conc_pm25$scenario, conc_pm25$species, sep=" - ")
raster::plot(rs)
```

<img src="/private/var/folders/1v/d44g64ds6299wsbvbs60w1qr0000gp/T/Rtmpc2f6N5/preview-a2f167d5f621.dir/example_philippines_files/figure-gfm/unnamed-chunk-7-1.png" width="100%" />

## Read baseline concentrations

``` r
conc_baseline <- creahia::get_conc_baseline(species=species, grid_raster=grid_raster)
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj =
#> prefer_proj): Discarded ellps unknown in Proj4 definition: +proj=goode +lon_0=0
#> +x_0=0 +y_0=0 +R=6371007 +units=km +no_defs +type=crs
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj =
#> prefer_proj): Discarded datum unknown in Proj4 definition
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj =
#> prefer_proj): Discarded ellps unknown in Proj4 definition: +proj=goode +lon_0=0
#> +x_0=0 +y_0=0 +R=6371007 +units=km +no_defs +type=crs
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj =
#> prefer_proj): Discarded datum unknown in Proj4 definition
conc_baseline
#> # A tibble: 3 x 2
#>   species conc_baseline     
#>   <chr>   <named list>      
#> 1 pm25    <RastrLyr[,863,1]>
#> 2 no2     <RastrLyr[,863,1]>
#> 3 so2     <RastrLyr[,863,1]>
```

## Combine both

``` r
concs <- creahia::combine_concs(conc_perturbation, conc_baseline) %>%
  flatten_concs() %>% # Make it one row per scenario
  add_pop() # Add popilation data
#> Warning in .local(x, ...): This function is only useful for Raster* objects with
#> a longitude/latitude coordinates
```

## Select regions of interest

We select regions of interest from GADM dataset, at the administrative
level 2.

``` r
regions <- creahia::get_adm(grid_raster, admin_level=2)
#> Warning in wkt(pto): CRS object has no comment
#> Warning in rgdal::rawTransform(projfrom, projto, nrow(xy), xy[, 1], xy[, : Using
#> PROJ not WKT2 strings
#> Warning in wkt(obj): CRS object has no comment

#> Warning in wkt(obj): CRS object has no comment
#> Warning in rgeos::gBuffer(adm_4326, byid = TRUE, width = 0): Spatial object is
#> not projected; GEOS expects planar coordinates
#> Warning in spTransform(xSP, CRSobj, ...): NULL source CRS comment, falling back
#> to PROJ string
#> Warning in wkt(obj): CRS object has no comment
regions <- regions[1:5,] # For example purposes, we only keep a subset
```

and extract concentrations there:

``` r
conc_regions <- creahia::extract_concs_at_regions(concs, regions)
```

# Health Impact Assessment

``` r
hia <- compute_hia(conc_map=conc_regions,
                   species=species,
                   regions=regions)
#> [1] "Computing paf"
#> [1] "Getting adult_ages"
#> [1] "Getting IHME"
#> Warning in age_name %>% gsub(" .*", "", .) %>% as.numeric: NAs introduced by
#> coercion
#> Warning: executing %dopar% sequentially: no parallel backend registered
#> [1] "Getting calc_causes"
#> [1] "Getting GBD"
#> [1] "Getting GEMM"
#> [1] "Computing epi"
#> [1] "Getting EPI"
#> [1] "Getting CRFS"
#> [1] "opr"
#> [1] "oprnew"
hia
#>    scenario   region_id estimate NCD.LRI_YLLs_PM25 LRI.child_YLLs_PM25
#> 1       opr  IDN.29.7_1  central      0.0006760297        1.970398e-05
#> 2       opr  MYS.13.4_1  central      0.0548289975        4.026829e-04
#> 3       opr MYS.13.11_1  central      0.0611353911        4.844843e-04
#> 4       opr  IDN.29.7_1     high      0.0009341406        4.511514e-05
#> 5       opr  MYS.13.4_1     high      0.0812518541        8.600140e-04
#> 6       opr MYS.13.11_1     high      0.0905973660        1.040107e-03
#> 7       opr  IDN.29.7_1      low      0.0004621937        0.000000e+00
#> 8       opr  MYS.13.4_1      low      0.0345195598        1.056541e-04
#> 9       opr MYS.13.11_1      low      0.0384899845        2.175478e-04
#> 10   oprnew  IDN.29.7_1  central      0.0006939486        2.022633e-05
#> 11   oprnew  MYS.13.4_1  central      0.0654033189        4.809422e-04
#> 12   oprnew MYS.13.11_1  central      0.0733413609        5.812437e-04
#> 13   oprnew  IDN.29.7_1     high      0.0009589011        4.631114e-05
#> 14   oprnew  MYS.13.4_1     high      0.0969221190        1.026665e-03
#> 15   oprnew MYS.13.11_1     high      0.1086855896        1.248414e-03
#> 16   oprnew  IDN.29.7_1      low      0.0004744447        0.000000e+00
#> 17   oprnew  MYS.13.4_1      low      0.0411769895        1.273653e-04
#> 18   oprnew MYS.13.11_1      low      0.0461746786        2.613583e-04
#>    Stroke_YLDs_PM25 Diabetes_YLDs_PM25 COPD_YLDs_PM25 NCD.LRI_Deaths_PM25
#> 1      1.545765e-05       6.470941e-05   1.023995e-05        2.680489e-05
#> 2      1.141614e-03       8.627364e-03   9.199453e-04        2.359730e-03
#> 3      1.328155e-03       1.001291e-02   1.062177e-03        2.631145e-03
#> 4      3.163412e-05       9.319316e-05   1.902453e-05        3.604948e-05
#> 5      2.265878e-03       1.454745e-02   1.715325e-03        3.444581e-03
#> 6      2.636124e-03       1.620594e-02   1.980528e-03        3.840773e-03
#> 7      4.996362e-06       0.000000e+00   3.729223e-06        1.847829e-05
#> 8      3.790912e-04       1.660299e-03   3.311089e-04        1.503615e-03
#> 9      4.410353e-04       3.613141e-03   3.823015e-04        1.676560e-03
#> 10     1.586742e-05       6.642484e-05   1.051140e-05        2.751539e-05
#> 11     1.362597e-03       1.029267e-02   1.097903e-03        2.814827e-03
#> 12     1.593519e-03       1.201751e-02   1.274370e-03        3.156465e-03
#> 13     3.247272e-05       9.566370e-05   1.952885e-05        3.700502e-05
#> 14     2.704488e-03       1.735851e-02   2.047145e-03        4.108903e-03
#> 15     3.162821e-03       1.943543e-02   2.376184e-03        4.607603e-03
#> 16     5.128810e-06       0.000000e+00   3.828078e-06        1.896807e-05
#> 17     4.524718e-04       2.003228e-03   3.951594e-04        1.793602e-03
#> 18     5.291532e-04       4.342511e-03   4.586742e-04        2.011293e-03
#>    IHD_Deaths_PM25 Stroke_Deaths_PM25 COPD_Deaths_PM25 LC_Deaths_PM25
#> 1     9.586692e-06       4.659730e-06     1.490204e-06   1.117030e-06
#> 2     1.025025e-03       2.274555e-04     1.073521e-04   9.960649e-05
#> 3     1.145341e-03       2.646220e-04     1.239497e-04   1.151505e-04
#> 4     1.217415e-05       8.102381e-06     2.744623e-06   2.015400e-06
#> 5     1.424727e-03       4.567525e-04     2.281165e-04   1.949888e-04
#> 6     1.591959e-03       5.313862e-04     2.633852e-04   2.254175e-04
#> 7     7.174962e-06       1.839010e-06     5.530034e-07   4.654469e-07
#> 8     7.135900e-04       7.995993e-05     3.613660e-05   4.055771e-05
#> 9     7.973501e-04       9.302551e-05     4.172366e-05   4.688693e-05
#> 10    9.840803e-06       4.783255e-06     1.529707e-06   1.146641e-06
#> 11    1.222750e-03       2.714842e-04     1.281187e-04   1.188768e-04
#> 12    1.374023e-03       3.174931e-04     1.487114e-04   1.381549e-04
#> 13    1.249685e-05       8.317168e-06     2.817379e-06   2.068826e-06
#> 14    1.699554e-03       5.451666e-04     2.722445e-04   2.327124e-04
#> 15    1.909815e-03       6.375568e-04     3.160025e-04   2.704509e-04
#> 16    7.365145e-06       1.887760e-06     5.676627e-07   4.777852e-07
#> 17    8.512394e-04       9.543774e-05     4.312695e-05   4.840415e-05
#> 18    9.565511e-04       1.116118e-04     5.005884e-05   5.625381e-05
#>    LRI_Deaths_PM25 LRI.child_Deaths_PM25 Diabetes_Deaths_PM25       pop
#> 1     1.212641e-06          2.238361e-07         7.366315e-06   242.711
#> 2     6.694190e-04          4.563233e-06         1.882915e-04 29961.643
#> 3     7.994660e-04          5.490212e-06         2.185309e-04 38687.521
#> 4     2.183288e-06          5.124182e-07         9.059171e-06   242.711
#> 5     1.352247e-03          9.746288e-06         3.048291e-04 29961.643
#> 6     1.614945e-03          1.178723e-05         3.395812e-04 38687.521
#> 7     4.551567e-07          0.000000e+00         0.000000e+00   242.711
#> 8     1.936299e-04          1.197461e-06         4.093445e-05 29961.643
#> 9     2.312463e-04          2.465640e-06         8.908152e-05 38687.521
#> 10    1.244789e-06          2.297699e-07         7.561595e-06   242.711
#> 11    7.993006e-04          5.450073e-06         2.246367e-04 29961.643
#> 12    9.592723e-04          6.586696e-06         2.622812e-04 38687.521
#> 13    2.241170e-06          5.260024e-07         9.299329e-06   242.711
#> 14    1.614614e-03          1.163489e-05         3.637325e-04 29961.643
#> 15    1.937761e-03          1.414792e-05         4.072524e-04 38687.521
#> 16    4.672232e-07          0.000000e+00         0.000000e+00   242.711
#> 17    2.311979e-04          1.443532e-06         4.938931e-05 29961.643
#> 18    2.774700e-04          2.962179e-06         1.070640e-04 38687.521
#>    Asthma.Inci.1to18_NO2 Asthma.Prev.1to18_NO2 exac.0to17_PM25 exac.18to99_PM25
#> 1           3.085101e-05          1.320043e-04    0.000000e+00     0.0000000000
#> 2           1.588529e-04          6.944519e-04    1.797131e-04     0.0002431626
#> 3           6.161463e-06          2.693585e-05    4.618345e-04     0.0006248898
#> 4           6.735102e-05          2.685626e-04    0.000000e+00     0.0000000000
#> 5           3.488478e-04          1.492736e-03    2.644241e-04     0.0003264612
#> 6           1.353084e-05          5.789908e-05    6.795283e-04     0.0008389539
#> 7           7.067670e-06          3.514862e-05    0.000000e+00     0.0000000000
#> 8           3.513481e-05          1.766678e-04    9.400446e-05     0.0001592100
#> 9           1.362780e-06          6.852449e-06    2.415768e-04     0.0004091449
#> 10          3.113982e-05          1.332400e-04    0.000000e+00     0.0000000000
#> 11          1.985143e-04          8.678380e-04    2.173448e-04     0.0002940805
#> 12          7.918431e-06          3.461673e-05    5.556547e-04     0.0007518342
#> 13          6.798150e-05          2.710767e-04    0.000000e+00     0.0000000000
#> 14          4.359457e-04          1.865432e-03    3.197941e-04     0.0003948217
#> 15          1.738921e-05          7.440925e-05    8.175723e-04     0.0010093847
#> 16          7.133833e-06          3.547765e-05    0.000000e+00     0.0000000000
#> 17          4.390704e-05          2.207771e-04    1.136889e-04     0.0001925484
#> 18          1.751383e-06          8.806454e-06    2.906524e-04     0.0004922615
#>    PTB_PM25     LBW_PM25 NCD.LRI_Deaths_NO2 NCD.LRI_YLLs_NO2 Absences_PM25
#> 1         0 6.454490e-06                  0                0   0.007555748
#> 2         0 7.177478e-04                  0                0   0.797479790
#> 3         0 8.487365e-04                  0                0   0.943019193
#> 4         0 1.120872e-05                  0                0   0.008676315
#> 5         0 1.246426e-03                  0                0   0.915751267
#> 6         0 1.473899e-03                  0                0   1.082875157
#> 7         0 2.001759e-06                  0                0   0.006427657
#> 8         0 2.225980e-04                  0                0   0.678414053
#> 9         0 2.632220e-04                  0                0   0.802224027
#> 10        0 6.625597e-06                  0                0   0.007756049
#> 11        0 8.568805e-04                  0                0   0.952068749
#> 12        0 1.018357e-03                  0                0   1.131483264
#> 13        0 1.150586e-05                  0                0   0.008906322
#> 14        0 1.488039e-03                  0                0   1.093266688
#> 15        0 1.768457e-03                  0                0   1.299289587
#> 16        0 2.054825e-06                  0                0   0.006598052
#> 17        0 2.657480e-04                  0                0   0.809922561
#> 18        0 3.158274e-04                  0                0   0.962550010
#>    NCD.LRI_Deaths_SO2 NCD.LRI_YLLs_SO2 Deaths_Total      region_name iso3
#> 1        5.826466e-07     1.419054e-05 2.761138e-05 Kepulauan Talaud  IDN
#> 2        4.876664e-06     1.121284e-04 2.369170e-03     Kinabatangan  MYS
#> 3        5.265585e-06     1.210708e-04 2.641901e-03       Lahad Datu  MYS
#> 4        8.508554e-07     2.137872e-05 3.741276e-05 Kepulauan Talaud  IDN
#> 5        7.647000e-06     1.786352e-04 3.461974e-03     Kinabatangan  MYS
#> 6        8.256858e-06     1.928816e-04 3.860817e-03       Lahad Datu  MYS
#> 7        3.490399e-07     8.413594e-06 1.882733e-05 Kepulauan Talaud  IDN
#> 8        2.818898e-06     6.375925e-05 1.507632e-03     Kinabatangan  MYS
#> 9        3.043709e-06     6.884414e-05 1.682070e-03       Lahad Datu  MYS
#> 10       5.826661e-07     1.419101e-05 2.832782e-05 Kepulauan Talaud  IDN
#> 11       8.918388e-06     2.050591e-04 2.829196e-03     Kinabatangan  MYS
#> 12       9.527867e-06     2.190728e-04 3.172580e-03       Lahad Datu  MYS
#> 13       8.508840e-07     2.137944e-05 3.838190e-05 Kepulauan Talaud  IDN
#> 14       1.398475e-05     3.266860e-04 4.134523e-03     Kinabatangan  MYS
#> 15       1.494046e-05     3.490116e-04 4.636691e-03       Lahad Datu  MYS
#> 16       3.490516e-07     8.413877e-06 1.931713e-05 Kepulauan Talaud  IDN
#> 17       5.155168e-06     1.166022e-04 1.800200e-03     Kinabatangan  MYS
#> 18       5.507471e-06     1.245707e-04 2.019763e-03       Lahad Datu  MYS
```

Summarise HIA:

    hia_table <- hia %>% totalise_hia() %>% make_hia_table()
    hia_table
