devtools::install_github('laurimyllyvirta/lauR')
require(lauR)
loadlibs(online=F)
require(ncdf4)

source("R/puffvis.R")

resultsdir = 'sample data/'
resultsdir %>% setwd

#process CALPUFF results
fileSpecs <- creapuff::get_calpuff_files(ext=".csv", gasunit = 'ug')

makeGrid(UTMz=51, UTMhem='N', mapres=1)
makeTifs(fileSpecs, nmax=8, idp=1.5,
         queue=which(fileSpecs$period == 'annual' | !is.na(fileSpecs$threshold)),
         overwrite = F)
adm_level=2

fileSpecs <- creapuff::get_calpuff_files(ext=".tif", gasunit = 'ug')

fileSpecs$name[1] %>% raster %>% raster %>% fixproj -> gridR
makeMap(gridR, admin_level=adm_level)

read_xlsx('PH Coal plants.xlsx', sheet='All', skip=1, n_max=45) -> CFPPs

HIArasterpath=boxpath('GIS/HIA/')
#files that need to be in this folder are found at:
#https://drive.google.com/drive/folders/1ze78ZzIgh_2oZ8V9KddAGEJdko5AT3M-?usp=sharing

#source of PM2.5 data: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
PM25.base <- paste0(HIArasterpath, 'GlobalGWRcwUni_PM25_GL_201601_201612-RH35_Median.nc') %>% raster
pop <- paste0(HIArasterpath, 'gpw_v4_population_density_rev11_2020_30_sec.tif') %>% raster
grump <- paste0(HIArasterpath, 'GRUMPv1/glurextents.bil') %>% raster
crs(grump) <- crs(pop)
#source of NO2 data: https://data.world/datasets/no2
NO2.base <- paste0(HIArasterpath, 'no2_agg8.grd') %>% raster

admLL <- getadm(adm_level)

#aggregate population grid
gridR %>% projectExtent(pop) %>% crop(pop, .) -> pop
pop[is.na(pop)] <- 0

#adjust NO2 concentrations using OMI data for 2011 and 2019
paste0(HIArasterpath, "NO2_2010-2011.nc") %>% raster %>% cropProj(gridR) -> NO2.11
paste0(HIArasterpath, "NO2_2018-2019.nc") %>% raster %>% cropProj(gridR) -> NO2.19
NO2.11 %>% focal(focalWeight(., 100, "circle"), mean, na.rm=T, pad=T, padValue=NA) -> NO2.11.smooth
NO2.19 %>% focal(focalWeight(., 100, "circle"), mean, na.rm=T, pad=T, padValue=NA) -> NO2.19.smooth
NO2.ratio=NO2.19.smooth/NO2.11.smooth
NO2.ratio %<>% max(min_incr)

#specify function that returns the concentration grid for a specific scenario and pollutant
get.grid <- function(.scenario, .speciesName, .period='annual') {
  fileSpecs %>% subset(scenario==.scenario &
                         speciesName==.speciesName &
                         period==.period,
                       select=name, drop=T) %>%
    raster %>% fixproj
}

rm(gridR) #will be re-created inside the loop
scens = unique(fileSpecs$scenario)
scens %>% seq_along() -> queue

for(s in scens[queue]) {
  #initialize grid and maps
  if(!exists('gridR')) {
    get.grid(s, 'PM2.5') %>% raster -> gridR

    admLL %>% cropProj(gridR) -> admUTM
    admUTM$ID <- admUTM[[paste0('GID_',adm_level)]]

    pop %>% cropProj(gridR) -> popDP
    popDP %>% multiply_by(area(popDP)) -> popCP

    #create a stack of concentration grids
    PM25.base %>% cropProj(gridR) -> PM25.baseUTM
    NO2.base %>% cropProj(gridR) %>% multiply_by(NO2.ratio*2.1) -> NO2.baseUTM
    NO2.baseUTM %<>% multiply_by(1.88)
    NO2.baseUTM[] %<>% na.approx(maxgap=5, na.rm=F)
    SO2.baseUTM=gridR
    values(SO2.baseUTM)=1

    o3.base %>% cropProj(gridR) -> O3.baseUTM
    o3.coal %>% cropProj(gridR) -> O3.coalUTM
  }


  get.grid(s, 'PM2.5') -> PM25.coalUTM
  PM25.coalUTM %<>% add(PM25.baseUTM)

  get.grid(s, 'NO2') -> NO2.coalUTM
  NO2.coalUTM %<>% add(NO2.baseUTM)

  get.grid(s, 'SO2') -> SO2.coalUTM
  SO2.coalUTM %<>% add(SO2.baseUTM)

  list(PM25.base=PM25.baseUTM,
       PM25.coal=PM25.coalUTM,
       pop=popCP,
       NO2.base=NO2.baseUTM,
       NO2.coal=NO2.coalUTM,
       SO2.base=SO2.baseUTM,
       SO2.coal=SO2.coalUTM) %>% stack -> concR

  #extract and save concentration data
  raster::extract(concR, admUTM) -> conc_adm
  names(conc_adm) <- admUTM$ID

  conc_adm %<>% subset(!sapply(., is.null))
  conc_adm %<>% lapply(na.fill, 0)

  saveRDS(conc_adm, paste0(s, '-conc_adm.RDS'))
  print(s)
}


#read HIA input data
HIApath='../data/'

source(paste0(HIApath, 'helper functions.R'))
source(paste0(HIApath, 'gemm_function.R'))
source(paste0(HIApath, 'read HIA data.R'))
creahelpers::get_population_path('WPP2019_population-death_rate-birth_rate.csv') %>% read_csv() %>%
  mutate(deaths=pop*death_rate) -> popproj

crfs$Incidence[crfs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', crfs$Incidence)] %<>%
  gsub('NCD\\.LRI', 'AllCause', .)


#calculate country average death, YOLL, YLD PAF values for baseline and fossil cases
hia_all <- list()
paf_all <- list()
HIApolls <- c("PM25", "NO2", "SO2")

queue <- scens

for(s in queue) {
  conc_adm <- readRDS(paste0(s, '-conc_adm.RDS'))
  #names(conc_adm) %<>% country.recode(merge_into)
  conc_adm %<>% ldply(.id='GID') %>% dlply(.(GID))
  paf = list()
  for(cy in names(conc_adm)) {
    paf[[cy]] <- list()

    for(cs_ms in calc_causes) {
      cs_ms %>% strsplit('_') %>% unlist -> cs.ms
      epi_country <- substr(cy, 1, 3)
      country.paf.perm(pm.base = conc_adm[[cy]][, 'PM25.base'],
                       pm.perm = conc_adm[[cy]][, 'PM25.coal'],
                       pop = conc_adm[[cy]][, 'pop'],
                       cy=epi_country,
                       cause=cs.ms[1],
                       measure=cs.ms[2],
                       .region="inc_China") -> paf[[cy]][[cs_ms]]
    }
    paf[[cy]] %<>% ldply(.id='var')
    print(length(paf))
  }

  paf %<>% ldply(.id='GID')
  paf_all[[s]] <- paf
  print(length(paf_all))
}

saveRDS(paf_all, 'paf_all.RDS')

queue <- scens

for(s in queue) {
  conc_adm <- readRDS(paste0(s, '-conc_adm.RDS'))
  #names(conc_adm) %<>% country.recode(merge_into)
  conc_adm %<>% ldply(.id='GID') %>% dlply(.(GID))

  #calculate health impacts
  conc_adm %>% ldply(function(df) df %>% sel(-GID) %>% apply(2, weighted.mean, w=df[,'pop']) %>% t %>%
                    data.frame %>% mutate(pop=sum(df[,'pop'])), .id='GID') -> domainpop

  domainpop$GID %>% substr(1,3) %>% as.character %>% country.recode(use_as_proxy) -> domainpop$epi_ISO3

  epi %>% sel(-pop, -country) %>% rename(epi_ISO3=ISO3) %>% filter(epi_ISO3 %in% domainpop$epi_ISO3) %>%
    full_join(domainpop %>% sel(GID, epi_ISO3, pop)) %>% sel(-epi_ISO3) -> epi_loc

  epi_loc %>% sel(GID, estimate, pop) -> hia

  for(i in which(crfs$Exposure %in% HIApolls)) {
    crfs$Exposure[i] -> concname

    if(grepl('nrt', concname)) {
      sourceconcs <- get.nrt.conc(hia$GID, concname, 0, conc_adm = conc_adm)
    } else {
      concname %>% paste0('.base') -> basename
      concname %>% paste0('.coal') -> permname
      nrt.flag = NULL #ifelse(grepl('NCD\\.LRI_', crfs$Incidence[i]), 'grump', NULL)

      crfs$Counterfact[i] -> cfconc
      get.nrt.conc(hia$GID, basename, cfconc, crfs$Units.multiplier[i], nrt.flag=nrt.flag, conc_adm = conc_adm) -> baseconcs
      get.nrt.conc(hia$GID, permname, cfconc, crfs$Units.multiplier[i], nrt.flag=nrt.flag, conc_adm = conc_adm) -> permconcs

      sourceconcs <- permconcs - baseconcs
    }

    match(hia$estimate, names(crfs)) -> RR.ind
    crfs[i, RR.ind] %>% unlist %>% unname -> RRs

    epi_loc[[crfs$Incidence[i]]] / 1e5 * epi_loc$pop * (1 - exp(-log(RRs)*sourceconcs / crfs$Conc.change[i])) ->
      hia[[crfs$effectname[i]]]
  }

  #calculate PM mortality
  paf_all[[s]] %>% gather(estimate, val, low, central, high) %>%
    mutate(var=paste0('paf_', var)) %>%
    spread(var, val) -> paf_wide
  epi_loc %>% left_join(paf_wide) -> paf_wide

  paf_wide %>% sel(GID, estimate) -> pm_mort

  for(cs in calc_causes)
    paf_wide[[cs]] / 1e5 * paf_wide[[paste0('paf_', cs)]] * paf_wide$pop -> pm_mort[[cs]]

  names(pm_mort)[sapply(pm_mort, is.numeric)] %<>% paste0('_PM25')
  full_join(pm_mort, hia) -> hia_all[[s]]
  print(s)
}

hia_all %<>% ldply(.id='scenario') %>% mutate(ISO3 = GID %>% substr(1, 3))

#scale population from year of population data to target year of estimates
pop_baseyr=2015
pop_targetyr=2019
popproj %>% filter(Yr %in% c(pop_baseyr, pop_targetyr), age_group != 'Newborn', !is.na(ISO3)) %>%
  sel(-age_group) %>% group_by(ISO3, Yr) %>% summarise_at('pop', sum) -> popscaling
popscaling$Yr[popscaling$Yr==pop_baseyr] <- 'base'
popscaling$Yr[popscaling$Yr==pop_targetyr] <- 'target'
popscaling %>% spread(Yr, pop) %>% mutate(scaling=target/base) %>% sel(ISO3, scaling) %>%
  left_join(hia_all, .) %>%
  (function(df) df %>% mutate_if(is.numeric, multiply_by, df$scaling)) %>% sel(-scaling) ->
  hia_all

saveRDS(hia_all, 'hia_all.RDS')

hia_all %>% mutate(Deaths_Total =
                     NCD.LRI_Deaths_PM25 +
                     NCD.LRI_Deaths_SO2 +
                     NCD.LRI_Deaths_NO2 +
                     COPD_Deaths_O3_8h +
                     LRI.child_Deaths_PM25) %>%
  group_by(GID, scenario, estimate) %>% summarise_if(is.numeric, sum, na.rm=T) %>%
  gather(outcome, Number, -GID, -scenario, -estimate) -> hia_adm

names(hia_adm) %<>% recode(GID=paste0('GID_',adm_level))
hia_adm %<>% left_join(admUTM@data %>% sel(starts_with('GID_'), starts_with('NAME_')))

hia_adm %>% group_by(scenario, estimate, outcome) %>% summarise_if(is.numeric, sum, na.rm=T) %>%
  spread(estimate, Number) -> hia_tot

read_csv(paste0(HIApath, 'dict.csv')) -> dict

hia_tot %>% maketable -> hiatable
hiatable %>% write_csv('HIAtable.csv')

#economic cost
hia_all %>% gather(outcome, number, -scenario, -GID, -ISO3, -estimate, -pop) %>%
  mutate(ISO3 = GID %>% substr(1, 3),
         outcome = outcome %>%gsub('O3_8h', 'O3', .),
         pollutant = outcome %>% gsub('.*_', '', .) %>% toupper,
         cause = outcome %>% gsub('_.*', '', .),
         outcome = outcome %>% gsub('_[A-Za-z0-9]*$', '', .) %>%
           gsub('\\.[0-9]*to[0-9]*$', '', .) %>% gsub('.*_', '', .)) ->
  hia2

hia2$cause[grep('exac|sthma', hia2$cause)] <- 'Asthma'

hia2 %<>% left_join(vals) %>% left_join(gdp) %>%
  filter(!is.na(pollutant)) %>%
  mutate(valuation = Valuation.2011.IntlDollars * (GDP.PPP.2011USD / 15914.05317)^Elasticity,
         cost=number*valuation/1e6,
         cost.USD = cost * GDP.currUSD / GDP.PPP.2011USD,
         cost.LCU = cost * GDP.currLCU / GDP.PPP.2011USD) %>%
  ungroup

hia2 %>% group_by(scenario, estimate, outcome, pollutant) %>% summarise_at('cost.USD', sum) %>% na.omit %>%
  filter(outcome != 'LBW') %>% spread(estimate, cost.USD) %>%
  mutate_at(rescols, scales::comma, accuracy=0.01) %>% mutate(CI = paste0('(', low, ' - ', high, ')')) %>%
  sel(-low, -high) %>% write_csv('total costs by cause, mln USD.csv')


hia2 %>% filter(outcome != 'LBW') %>%
  group_by(scenario, ISO3, GID, estimate, Currency.Name, Currency.Code) %>%
  sel(starts_with('cost')) %>% summarise_all(sum, na.rm=T) %>%
  left_join(hia2 %>% distinct(GID, pop, GDP.PPP.2011USD)) %>%
  mutate(cost.percap.USD = cost.USD * 1e6 / pop,
         cost.perc = cost * 1e6 / (GDP.PPP.2011USD * pop)) -> cost_totals

cost_totals %>% right_join(admUTM@data %>% sel(GID=GID_2, starts_with('NAME_')), .) %>%
  arrange(estimate, desc(cost.perc)) %>% sel(-starts_with('GDP'), -starts_with('valuation'), -cost) %>%
  write_csv('cost results by adm 2 area.csv')

cost_totals %>% group_by(scenario, ISO3, estimate) %>%
  summarise_at(c('cost.LCU', 'cost.USD'), sum) -> cost_natl

cost_natl %>% write_csv('cost results national total.csv')

#valuations used
output_vals_for='Philippines'
currency_name=gdp$Currency.Code[gdp$ISO3==get_iso3(output_vals_for)]
hia2 %>% distinct(ISO3, outcome, .keep_all=T) %>%
  left_join(admUTM@data %>% sel(ISO3=GID_0, Country='NAME_0')) %>%
  filter(Country==output_vals_for) %>%
  mutate(valuation.USD = valuation * GDP.currUSD / GDP.PPP.2011USD,
         valuation.LCU = valuation * GDP.currLCU / GDP.PPP.2011USD) %>%
  sel(outcome.Code=outcome,
      Valuation.at.world.avg.GDP.2011.IntlDollars=Valuation.2011.IntlDollars,
      Valuation.in.COUNTRY.2011.IntlDollars=valuation,
      Valuation.in.COUNTRY.2019USD=valuation.USD,
      Valuation.in.COUNTRY.2019LCU=valuation.LCU) %>% distinct() %>% na.omit() %>%
  rename_with(function(x) x %>% gsub('COUNTRY', output_vals_for, .) %>% gsub('LCU', currency_name, .)) %>%
  filter(outcome.Code != 'LBW') %>%
  right_join(dict %>% rename(outcome.Code=Code, outcome=Long.name), .) %>% sel(-outcome.Code) %>%
  write_excel_csv('valuations in focus country.csv')


#future impacts
hia2 %>% filter(outcome != 'LBW',
                outcome %notin% c('Deaths', 'YLLs') | cause %in% c('NCD.LRI', 'LRI.child', 'AllCause'),
                outcome!='YLDs' | cause != 'NCD.LRI') -> hia3

names(hia3) %<>% recode('GID' = paste0('GID_',adm_level))
admLL@data %>% sel(starts_with('GID_'), starts_with('NAME_')) %>%
  right_join(hia3) -> hia3
hia3 %>% group_by(scenario, fuel, area, estimate, ISO3, NAME_1, outcome, cause, age_group, pollutant) %>%
  summarise_at(c('number', 'cost.USD'), sum, na.rm=T) -> hia_cost

#add new age groups to population data
add_age_groups = tibble(age_group=c('25+','0-18','1-18','18-99', '20-65'),
                        AgeLow=c(25,0,0,20, 20),
                        AgeHigh=c(99,20,99, 99, 64),
                        multiplier=c(1,19/20,18/20, 82/80, 46/45))

add_age_groups %>% group_by(age_group) %>%
  group_modify(function(df, ...) {
    popproj %>% filter(Age_low>=df$AgeLow, Age_high<=df$AgeHigh) %>%
      group_by(LocID, ISO3, Location, Yr) %>% sel(-contains('Age')) %>%
      mutate_if(is.numeric, multiply_by, df$multiplier) %>%
      summarise_all(sum) %>%
      mutate(death_rate = deaths / pop, age_group=df$age_group)
  }) %>% bind_rows(popproj) %>% distinct -> popproj

#flag mortality outcomes (to be scaled by number of deaths)
hia_cost$fatal <- grepl('YLLs|YLDs|Deaths', hia_cost$outcome)
yrs = 1980:2060

#gdp data
require(wbstats)
list(GDP.PPP.2011USD = 'NY.GDP.PCAP.PP.KD',
     GDP.currUSD     = 'NY.GDP.PCAP.CD',
     GDP.currLCU     = 'NY.GDP.PCAP.CN',
     GDP.PPP.tot     = 'NY.GDP.MKTP.PP.KD') %>%
  lapply(readWB_online, start_date = 1980, end_date = 2020, latest.year.only=F) %>%
  bind_rows(.id='valuename') %>% sel(country, ISO3, Year, valuename, Value) %>%
  spread(valuename, Value) -> GDP_historical

futgdpF=paste(HIApath, 'OECD_GDP_forecast.csv')
if(!file.exists(futgdpF))
  download.file('https://stats.oecd.org/sdmx-json/data/DP_LIVE/.GDPLTFORECAST.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en',
                futgdpF)
read_csv(futgdpF) %>% sel(ISO3=LOCATION, Year=TIME, GDP.realUSD.tot=Value) -> GDP_future
full_join(GDP_historical, GDP_future) -> GDP
GDP %<>% left_join(popproj_tot %>% rename(Year=Yr)) %>%
  mutate(GDP.realUSD = GDP.realUSD.tot*1000/pop) %>%
  group_by(ISO3) %>%
  group_modify(function(df, ...) {
    PPP.scaling = df$GDP.PPP.2011USD[df$Year==2019] / df$GDP.realUSD[df$Year==2019]
    if(length(PPP.scaling)>0)
      df %<>% mutate(GDP.realUSD = GDP.realUSD)

    past.scaling = df %>% filter(!is.na(GDP.PPP.2011USD+GDP.currUSD)) %>% head(1)
    ind=df$Year<past.scaling$Year
    df$GDP.PPP.2011USD[ind] %<>% na.cover(df$GDP.currUSD[ind] * past.scaling$GDP.PPP.2011USD / past.scaling$GDP.currUSD)

    future.scaling = df %>% filter(!is.na(GDP.PPP.2011USD+GDP.realUSD)) %>% tail(1)
    ind=df$Year>future.scaling$Year
    df$GDP.PPP.2011USD[ind] %<>% na.cover(df$GDP.realUSD[ind] * past.scaling$GDP.PPP.2011USD / past.scaling$GDP.realUSD)

    return(df)
  })

GDP %>% group_by(ISO3) %>%
  group_map(function(df, ISO3, ...) {
    df%<>%select_if(is.numeric)
    y1 = df %>% filter(Year==2019)
    y0 = df %>% filter(Year==2010)
    if(nrow(y0)==1 & nrow(y1)==1) { bind_cols(ISO3, y1/y0)
    } else NULL
  }) -> elast

elast %>% subset(!is.null(.)) %>% bind_rows %>%
  mutate(elast = (GDP.PPP.tot-1) / (GDP.realUSD.tot-1)) %>% summarise_at('elast', mean, na.rm=T)

popproj %>% ungroup %>%
  filter(ISO3 %in% unique(hia_cost$ISO3),
         age_group %in% unique(hia_cost$age_group), Yr %in% yrs) %>%
  full_join(GDP %>% sel(ISO3, Yr=Year, GDP.PPP.2011USD) %>%
              filter(Yr %in% yrs, ISO3 %in% unique(hia_cost$ISO3))) %>%
  pivot_longer(c(pop, deaths)) %>%
  group_by(ISO3, age_group, name) %>%
  mutate(scaling = value/value[Yr==pop_targetyr],
         GDPscaling = GDP.PPP.2011USD/GDP.PPP.2011USD[Yr==pop_targetyr]) %>%
  mutate(fatal=name=='deaths') %>% ungroup %>% sel(ISO3, age_group, Yr, fatal, scaling, GDPscaling) -> popscaling

hia_cost %>% full_join(popscaling) -> hia_by_year

hia_by_year %>% mutate(number = number*scaling,
                       cost.USD = cost.USD*scaling*GDPscaling) %>%
  group_by(scenario, fuel, area, estimate, ISO3, NAME_1, outcome, cause, pollutant, Yr) %>%
  summarise_at(c('number', 'cost.USD'), sum) -> hia_by_year_scaled

hia_by_year_scaled %>% filter(fuel == 'COAL', !is.na(scenario)) %>%
  mutate(NAME_1 = ifelse(ISO3=='KOR', NAME_1, 'All')) %>%
  group_by(across(c(where(is.character), Yr))) %>% summarise_all(sum, na.rm=T) %>%
  write_csv('health and cost results by year.csv')



#make plots
fileSpecs$name -> files

plotKM <- 400
CFPP_SP %>% extent %>% add(plotKM) -> plot_bb

CFPPs %<>% distinct(Lat, Long, .keep_all = T) %>%
  sel(Plant, Lat, Long, Status) %>% mutate(Source = Plant %>% gsub(' power station', '', ., ignore.case = T) %>%
                                             gsub(' Unit.*', '', .) %>%
                                             gsub(' U[0-9\\-]*', '', .)) %>%
  spdf %>% spTransform(crs(gridR))

cities <- 'citiesDistToLarge.shp' %>% paste0(HIApath, .) %>% shapefile()
cities %>% crop(gridLL) %>% spTransform(crs(gridR)) %>% crop(plot_bb * .8) %>%
  (function(sp) sp[c(order(-sp$dstTLrg)[1:8], which(sp$name == 'Islamkot')), ]) -> cityPlot
cityPlot$pos = ifelse(cityPlot@coords[, 1] < plot_bb@xmin + (plot_bb@xmax - plot_bb@xmin) * 1/3, 4, 2)
cityPlot$pos[cityPlot$name == 'Badin'] <- 1

#output maps
outputs <- c("png", "kml","expPop","cityconcs")

if("kml" %in% outputs) {
  #install.packages(c('plotrix', 'dismo', 'pixmap', 'RSAGA', 'colorRamps', 'aqp'))
  library(plotKML)
  if(!file.exists("zip.exe"))
    file.copy(paste0(HIApath, "zip.exe"),"zip.exe")
  labelF <- "factoryTransp3.png"
  if(!file.exists(labelF))
    file.copy(paste0(HIApath, labelF),labelF)
}

library(rasterVis)

if(!exists("adm0_UTM")) {
  getadm(0) %>% cropProj(gridR) -> adm0_UTM
}

expPop <- list()
popCP = makePop()

fileSpecs$scenarioName <- "Operating CFPPs"
fileSpecs$scenarioName[fileSpecs$scenario=='oprnew_a'] <- "Operating&Proposed CFPPs"
fileSpecs$titletxt <- NA
fileSpecs$titletxt <- paste0(ifelse(fileSpecs[["hr"]]<=24,paste0("Maximum ",fileSpecs[["hr"]],"-hour "),
                                    ifelse(fileSpecs[["type"]]=="concentration","Annual mean ","Annual total ")),
                             fileSpecs[["speciesName"]]," ",fileSpecs[["type"]],
                             "\nfrom ",fileSpecs[["scenarioName"]])

queue=which(fileSpecs$type=='concentration' & (fileSpecs$period == 'annual' |  !is.na(fileSpecs$threshold)))  #1:nrow(fileSpecs) #which(fileSpecs$scenario=='ppmine') #
test=F
fn.ext <- "" #add an extension to end of filenames to avoid overwriting

#set max value shown in graphs for each variable
colorkeybasis <- "opr_all"  #'matar1' #set NULL to set colorkey separately for each scenario


if(is.null(fileSpecs$k)) fileSpecs$k <- NA

if(!is.null(colorkeybasis)) {
  for(file in which(fileSpecs$scenario==colorkeybasis &
                    1:nrow(fileSpecs) %in% queue &
                    is.na(fileSpecs$k))) {
    rfile <- gsub(".csv",".tif",files[file])
    raster(rfile) -> conc_R
    prb = ifelse(fileSpecs$type[file] == 'deposition', .999625, 1)
    fileSpecs[fileSpecs$species == fileSpecs[file,"species"] &
                fileSpecs$period == fileSpecs[file,"period"]
              ,"k"] <- quantile(values(conc_R),probs=prb)
    print(files[file])

  }
}


#output maps
for(file in queue[ifelse(test,1,T)]) {
  rfile <- gsub(".csv",".tif",files[file])
  raster(rfile) %>% #crop(plot_bb) %>%
    disaggregate(2, method='bilinear') -> conc_R
  max(values(conc_R)) -> maxVal

  #k=ifelse(fileSpecs[file,"type"]=="deposition",quantile(values(conc_R),probs=.9995),max(values(conc_R)))
  if(is.null(colorkeybasis)) {
    k=quantile(values(conc_R),probs=.9995)
    fileSpecs[file,"k"] <- k
  } else fileSpecs[file,"k"] -> k

  thr <- fileSpecs[file,"threshold"]
  exceed <- !is.na(thr) & max(values(conc_R)) >= thr
  if(!is.na(thr)) print(paste("threshold", ifelse(exceed,"","not"),"exceeded for",fileSpecs[file,"titletxt"]))

  CFPP_SP -> CFPPplot

  if("png" %in% outputs) {
    plumeBreaks <- c(seq(0,1,1/40)^3,2000)
    if(fileSpecs[file,"species"] %in% c("pm25","tpm10","tsp")) {
      colRamp <- colorRampPalette(colors=c("white","gray","yellow","red","black"))(42)
      labelcol="blue"
      wetlandcol="purple"
    } else {
      colRamp <- colorRampPalette(colors=c("white","gray","cyan","blue","purple"))(42)
      labelcol="black"
      wetlandcol="red" }

    plumeBreaks <- plumeBreaks * k

    al <- seq(0,k, creahelpers::sigfloor(k/5))
    axislabels = list(at=al,labels=al)


    parSets = rasterTheme(region=colRamp)
    parSets$layout.widths = list(axis.key.padding = 0, ylab.right = 2)
    parSets$layout.widths$ylab.right = 2
    parSets$fontsize$text = 12*1.8; parSets$fontsize$points = 8*1.5
    parSets$axis.components$left$tck = 0
    parSets$axis.components$bottom$tck = 0
    parSets$axis.line=list(lwd=3)
    parSets$panel.background$col <- colRamp[length(colRamp)]

    outpng <- gsub("\\.csv|\\.tif",paste0("_levelplot",fn.ext,".png"),files[file])
    png(filename =  outpng,
        width = 3000, height = 2000, units = "px",
        bg = "white",res=200)

    pl <- levelplot(crop(conc_R,plot_bb),
                    margin=F,cex=.8,at=plumeBreaks[-length(plumeBreaks)],
                    par.settings=parSets,
                    main=fileSpecs[file,"titletxt"],ylab.right=fileSpecs[file,"unit"]) +
      layer(sp.lines(adm0_UTM, lwd=3, col='darkgray')) +
      layer(sp.points(CFPPplot, pch=24,lwd=1.5, col="white",fill="red",cex=.7)) +
      layer(sp.points(cityPlot, pch=1,lwd=3, col=labelcol)) +
      layer(sp.text(coordinates(cityPlot), txt = cityPlot$name,
                    pos = cityPlot$pos,col=labelcol,font=1, cex=.7))

    if(F) {
      pl = pl + layer(sp.text(textbuffer(coordinates(CFPPplot),width=1.2,steps=16),
                              txt = rep(CFPPplot$Source,16), pos = 4,font=2,cex=.6,col=rgb(1,1,1,alpha=1))) +
        layer(sp.text(coordinates(CFPPplot), txt = CFPPplot$Source, pos = 4,font=2,cex=.6,col="red")) +
        layer(sp.text(admlab@coords,
                      admlab %>% row.names,
                      col='maroon3', font=3))
    }

    print(pl)

    dev.off()
  }

  if("kml" %in% outputs | "expPop" %in% outputs) {

    lvls = unique(signif(c(k/10,seq(k/5,k,k/5)),1))

    if(!is.na(thr) & exceed) { #if a threshold has been specified, ensure it is included in levels
      lvls <- lvls[abs(lvls / thr - 1) > .1] #eliminate levels close to the threshold
      lvls <- sort(unique(c(lvls,thr)))
    }

    contP_UTM <- raster2contourPolys(conc_R,levels=lvls)[-1,]

    if("expPop" %in% outputs)
      expPop[[fileSpecs[file,"name"]]] <- data.frame(contP_UTM@data,
                                                     levelname=paste0(contP_UTM$level,fileSpecs[file,"unit"]),
                                                     pop=raster::extract(popCP,contP_UTM,sum,na.rm=T),
                                                     area=area(contP_UTM))

    if("kml" %in% outputs) {
      spTransform(contP_UTM,CRS(proj4string(gridLL))) -> contP
      outL <- paste0(gsub("\n"," ",fileSpecs[file,"titletxt"]),fn.ext)

      colorRampPalette(c("steelblue","yellow","orange","red","darkred"))(length(lvls)) -> yorb


      png("label.png",width=1000,height=100,pointsize=16,bg = "transparent")

      decimals <- lvls %>% subset(.>0) %>% min %>%
        log10 %>% -. %>% ceiling %>% max(0)
      legendlvls <- round(lvls, decimals)

      leg.title <- paste0(gsub(fn.ext, "", outL),
                          " (", fileSpecs[file,"unit"], ")")

      par(mar = rep(.5, 4))
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      legend("topleft", legend = legendlvls, col=yorb, pch = 15,
             xjust=0.5, yjust=0,horiz=T,
             title = leg.title,
             bg="white",
             text.width = 1.2*max(strwidth(legendlvls[-length(legendlvls)])))
      dev.off()

      kml_open(file.name=paste0(outL,".kml"))
      kml_layer(obj=contP, subfolder.name=fileSpecs[file,"unit"],
                colour=rank(contP@data$max),
                colour_scale=yorb,
                alpha=0.5,altitude=0,plot.labpt=F,
                labels=level,LabelScale=0.5)

      kml_layer(obj=CFPPplot, subfolder.name="Modeled sources",
                size=1,
                alpha=1,altitude=0,
                labels=Source,
                LabelScale=0.5,sname="labels",shape="factoryTransp3.png")

      kml_screen(image.file="label.png",position="UL",sname="Label")
      kml_close(file.name=paste0(outL,".kml"))
      zip(paste0(outL,".kmz"),c(paste0(outL,".kml"),"factoryTransp3.png","label.png"))
      if(!file.exists(paste0(outL,".kmz"))) stop("creating kmz failed")
      file.remove(paste0(outL,".kml"))
      file.remove("label.png")

    }
  }

}



if("expPop" %in% outputs) {
  expPop %>% ldply(.id='name') %>% left_join(fileSpecs %>% sel(name, titletxt, threshold)) -> expPop2
  expPop2 %>% filter(min >= threshold) %>% group_by(name, titletxt, threshold) %>%
    summarise_at(c('pop', 'area'), sum, na.rm=T) -> pop_exceed

  write_csv(expPop2, paste0("expPop_new_format",fn.ext,".csv"))
  write_csv(pop_exceed, paste0("threshold_exceedances",fn.ext,".csv"))

  outF <- file(paste0("expPop",fn.ext,".csv"),"w")
  names(expPop) <- gsub("\n"," ",fileSpecs[queue,"titletxt"])
  for(n in names(expPop)) {
    writeLines(n,outF)
    write.table(expPop[[n]],outF,append=T,row.names=F,quote=T,sep=",")
    writeLines("",outF)
  }
  close(outF)
}


#deposition totals
fileSpecs %>%
  subset(type=='deposition') -> depodf
depodf %>%
  subset(select='name',drop=T) %>%
  gsub('.csv','.tif',.) %>% stack -> depoR
crs(depoR) <- crs(gridR)

depoR * area(depoR) * 10^2 -> depoR #hectares to km2
names(depoR) <- paste0(depodf$species,'_',depodf$scenario)

#get deposition by land use type
raster(paste0(HIArasterpath,'ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')) -> lc
projectRaster(crop(lc,gridLL), gridR, method='ngb') -> lc.utm
read.csv(paste0(HIArasterpath,'ESACCI-LC-Legend.csv'),sep=';',stringsAsFactors = F) -> lc.leg

admUTM$ID <- admUTM$GID_0 %>% as.factor %>% as.numeric
landmask <- rasterize(admUTM, depoR, "ID")
lc.utm[lc.utm==210 & is.na(landmask)] <- 230

zonal(depoR, lc.utm, sum) -> depo.lc
depo.lc %>% as.data.frame %>%
  dplyr::rename(NB_LAB = zone) %>%
  left_join(lc.leg) -> depo.lc

depo.lc %>% write_csv('deposition by land use, detailed breakdown.csv')

depo.lc$broad.cat <- 'other'
depo.lc[depo.lc$NB_LAB %in% 10:30,'broad.cat'] <- 'cropland'
depo.lc[depo.lc$NB_LAB %in% c(50:100, 160:170),'broad.cat'] <- 'forest'
depo.lc[depo.lc$NB_LAB == 210,'broad.cat'] <- 'fresh water'
depo.lc[depo.lc$NB_LAB == 230,'broad.cat'] <- 'ocean'
depo.lc %>%
  group_by(broad.cat) %>%
  summarise_if(is.double,sum, na.rm=T) %>%
  dplyr::select(-NB_LAB) %>%
  gather(var, deposition, -broad.cat) %>%
  separate(var, c("pollutant", "scenario"), "_") %>%
  mutate(deposition = deposition / ifelse(pollutant=='hg',1e6, 1e3)) %>%
  mutate(unit = ifelse(pollutant=='hg', 'kg/yr', 't/yr')) -> deposums

deposums %>% write.csv('deposition by broad land use category.csv')

deposums %>% ungroup %>%
  group_by(ocean = (broad.cat == 'ocean'),
           pollutant, scenario, unit) %>%
  summarize_at('deposition', sum)

depoR %>% subset(grep('hg_', names(.))) %>% divide_by(100) -> hgdepo
hgdepo %>% as.list -> hgdepolist
names(hgdepolist) <- names(hgdepo)
hgdepolist %>%
  lapply(function(r) area(r)[r>125] %>% sum) %>% data.frame %>%
  write_csv('hg above 125, km2.csv')

#WDPA database extract
'~/GIS/WDPA/WDPA_Mar2020-Philippines.RDS' %>% readRDS() -> prot
prot %<>% spTransform(crs(gridR))

extract <- raster::extract
units = ifelse(grepl('hg', names(depoR)), 'mg', 'kg')
extract(depoR, prot, sum) %>% data.frame -> protdepo
names(protdepo) <- names(depoR) %>% paste0('_', units, '_total')
depoR %>% divide_by(area(.)) %>% extract(prot, mean) %>% data.frame -> protdepo_per
names(protdepo_per) <- names(depoR) %>% paste0('_', units, '_per.km2')
depoR %>% divide_by(area(.)) %>% extract(prot, max) %>% data.frame -> protdepo_maxper
names(protdepo_maxper) <- names(depoR) %>% paste0('_', units, '_maxper.km2')
protdepo %<>% bind_cols(protdepo_per, protdepo_maxper)
prot$NAME -> protdepo$name
protdepo %>% arrange(desc(hg_oprnew_a_mg_total)) %>% head

protdepo %>% write_csv('WDPA areas deposition.csv')

#query concentration values for cities
cityConcs <- crop(cityUTM,gridR)

files[queue] %>% stack %>% extract(cityConcs) %>% data.frame -> cc
names(cc) <- fileSpecs$titletxt[queue] %>% gsub('\n', ' ', .)
cityConcs@data %<>% bind_cols(cc)

cbind(cityConcs@coords,cityConcs@data) %>% write_csv(paste0("cityConcs.csv"))

