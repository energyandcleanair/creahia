library(creahia)

project_dir <- "example_ph"


# 01: Get coal additional concentrations from CALPUFF --------------------------------------
conc_coal_only <- get_conc_calpuff(dir=project_dir, utm_zone = 51, utm_hem = 'N', map_res =1)

species = unique(conc_coal_only$species)
scenarios = unique(conc_coal_only$scenario)
grid_raster = conc_coal_only$conc_coal_only[[1]] %>% raster


# 02: Get base concentration levels --------------------------------------------------------
conc_base <- get_conc_base(species=species, grid_raster=grid_raster)


# 03: Combine and flatten: one row per scenario --------------------------------------------
concs <- combine_concs(conc_coal_only, conc_base) %>% flatten_concs() %>% add_pop()


# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
adm <- get_map_adm(grid_raster, admin_level=2)
cities <- get_map_cities(grid_raster)


# 05: Extract concentrations ---------------------------------------------------------------
conc_adm <- extract_concs_at_map(concs, adm)
conc_cities <- extract_concs_at_map(concs, cities)

saveRDS(conc_adm, file.path(project_dir, "conc_adm.RDS"))
saveRDS(conc_cities, file.path(project_dir, "conc_cities.RDS"))

# If you start from here
# conc_adm <- readRDS(file.path(project_dir, "conc_adm.RDS"))
# scenarios <- names(conc_adm)


# 06: Get HIA data, in case you want to modify them.  ---------------------------------------
crfs <- get_crfs()
epi <- get_epi()
gemm <- get_gemm()
ihme <- get_ihme()
gbd <- get_gbd()


# 07: HIA Calculations: PAF -----------------------------------------------------------------
paf <- compute_hia_paf(conc_adm, calc_causes, gemm=gemm, gbd=gbd, ihme=ihme)
saveRDS(paf, file.path(project_dir, 'paf.RDS'))


# 08: HIA Calculations: EPI+PAF -------------------------------------------------------------
hia <- compute_hia_epi(species, paf, conc_map=conc_adm, epi=epi)
saveRDS(hia, file.path(project_dir, 'hia.RDS'))


#scale population from year of population data to target year of estimates
pop_baseyr=2015
pop_targetyr=2019
popproj %>% filter(Yr %in% c(pop_baseyr, pop_targetyr), AgeGrp != 'Newborn', !is.na(ISO3)) %>%
  sel(-AgeGrp) %>% group_by(ISO3, Yr) %>% summarise_at('pop', sum) -> popscaling
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
  gather(Outcome, Number, -GID, -scenario, -estimate) -> hia_adm

names(hia_adm) %<>% recode(GID=paste0('GID_',adm_level))
hia_adm %<>% left_join(admUTM@data %>% sel(starts_with('GID_'), starts_with('NAME_')))

hia_adm %>% group_by(scenario, estimate, Outcome) %>% summarise_if(is.numeric, sum, na.rm=T) %>%
  spread(estimate, Number) -> hia_tot

read_csv(paste0(HIApath, 'dict.csv')) -> dict

hia_tot %>% maketable -> hiatable
hiatable %>% write_csv('HIAtable.csv')

#economic cost
hia_all %>% gather(Outcome, number, -scenario, -GID, -ISO3, -estimate, -pop) %>%
  mutate(ISO3 = GID %>% substr(1, 3),
         Outcome = Outcome %>%gsub('O3_8h', 'O3', .),
         Pollutant = Outcome %>% gsub('.*_', '', .) %>% toupper,
         Cause = Outcome %>% gsub('_.*', '', .),
         Outcome = Outcome %>% gsub('_[A-Za-z0-9]*$', '', .) %>%
           gsub('\\.[0-9]*to[0-9]*$', '', .) %>% gsub('.*_', '', .)) ->
  hia2

hia2$Cause[grep('exac|sthma', hia2$Cause)] <- 'Asthma'

hia2 %<>% left_join(vals) %>% left_join(gdp) %>%
  filter(!is.na(Pollutant)) %>%
  mutate(valuation = Valuation.2011.IntlDollars * (GDP.PPP.2011USD / 15914.05317)^Elasticity,
         cost=number*valuation/1e6,
         cost.USD = cost * GDP.currUSD / GDP.PPP.2011USD,
         cost.LCU = cost * GDP.currLCU / GDP.PPP.2011USD) %>%
  ungroup

hia2 %>% group_by(scenario, estimate, Outcome, Pollutant) %>% summarise_at('cost.USD', sum) %>% na.omit %>%
  filter(Outcome != 'LBW') %>% spread(estimate, cost.USD) %>%
  mutate_at(rescols, scales::comma, accuracy=0.01) %>% mutate(CI = paste0('(', low, ' - ', high, ')')) %>%
  sel(-low, -high) %>% write_csv('total costs by cause, mln USD.csv')


hia2 %>% filter(Outcome != 'LBW') %>%
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
hia2 %>% distinct(ISO3, Outcome, .keep_all=T) %>%
  left_join(admUTM@data %>% sel(ISO3=GID_0, Country='NAME_0')) %>%
  filter(Country==output_vals_for) %>%
  mutate(valuation.USD = valuation * GDP.currUSD / GDP.PPP.2011USD,
         valuation.LCU = valuation * GDP.currLCU / GDP.PPP.2011USD) %>%
  sel(Outcome.Code=Outcome,
      Valuation.at.world.avg.GDP.2011.IntlDollars=Valuation.2011.IntlDollars,
      Valuation.in.COUNTRY.2011.IntlDollars=valuation,
      Valuation.in.COUNTRY.2019USD=valuation.USD,
      Valuation.in.COUNTRY.2019LCU=valuation.LCU) %>% distinct() %>% na.omit() %>%
  rename_with(function(x) x %>% gsub('COUNTRY', output_vals_for, .) %>% gsub('LCU', currency_name, .)) %>%
  filter(Outcome.Code != 'LBW') %>%
  right_join(dict %>% rename(Outcome.Code=Code, Outcome=Long.name), .) %>% sel(-Outcome.Code) %>%
  write_excel_csv('valuations in focus country.csv')


#future impacts
hia2 %>% filter(Outcome != 'LBW',
                Outcome %notin% c('Deaths', 'YLLs') | Cause %in% c('NCD.LRI', 'LRI.child', 'AllCause'),
                Outcome!='YLDs' | Cause != 'NCD.LRI') -> hia3

names(hia3) %<>% recode('GID' = paste0('GID_',adm_level))
admLL@data %>% sel(starts_with('GID_'), starts_with('NAME_')) %>%
  right_join(hia3) -> hia3
hia3 %>% group_by(scenario, fuel, area, estimate, ISO3, NAME_1, Outcome, Cause, AgeGrp, Pollutant) %>%
  summarise_at(c('number', 'cost.USD'), sum, na.rm=T) -> hia_cost

#add new age groups to population data
add_age_groups = tibble(AgeGrp=c('25+','0-18','1-18','18-99', '20-65'),
                        AgeLow=c(25,0,0,20, 20),
                        AgeHigh=c(99,20,99, 99, 64),
                        multiplier=c(1,19/20,18/20, 82/80, 46/45))

add_age_groups %>% group_by(AgeGrp) %>%
  group_modify(function(df, ...) {
    popproj %>% filter(Age_low>=df$AgeLow, Age_high<=df$AgeHigh) %>%
      group_by(LocID, ISO3, Location, Yr) %>% sel(-contains('Age')) %>%
      mutate_if(is.numeric, multiply_by, df$multiplier) %>%
      summarise_all(sum) %>%
      mutate(death_rate = deaths / pop, AgeGrp=df$AgeGrp)
  }) %>% bind_rows(popproj) %>% distinct -> popproj

#flag mortality outcomes (to be scaled by number of deaths)
hia_cost$fatal <- grepl('YLLs|YLDs|Deaths', hia_cost$Outcome)
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
         AgeGrp %in% unique(hia_cost$AgeGrp), Yr %in% yrs) %>%
  full_join(GDP %>% sel(ISO3, Yr=Year, GDP.PPP.2011USD) %>%
              filter(Yr %in% yrs, ISO3 %in% unique(hia_cost$ISO3))) %>%
  pivot_longer(c(pop, deaths)) %>%
  group_by(ISO3, AgeGrp, name) %>%
  mutate(scaling = value/value[Yr==pop_targetyr],
         GDPscaling = GDP.PPP.2011USD/GDP.PPP.2011USD[Yr==pop_targetyr]) %>%
  mutate(fatal=name=='deaths') %>% ungroup %>% sel(ISO3, AgeGrp, Yr, fatal, scaling, GDPscaling) -> popscaling

hia_cost %>% full_join(popscaling) -> hia_by_year

hia_by_year %>% mutate(number = number*scaling,
                       cost.USD = cost.USD*scaling*GDPscaling) %>%
  group_by(scenario, fuel, area, estimate, ISO3, NAME_1, Outcome, Cause, Pollutant, Yr) %>%
  summarise_at(c('number', 'cost.USD'), sum) -> hia_by_year_scaled

hia_by_year_scaled %>% filter(fuel == 'COAL', !is.na(scenario)) %>%
  mutate(NAME_1 = ifelse(ISO3=='KOR', NAME_1, 'All')) %>%
  group_by(across(c(where(is.character), Yr))) %>% summarise_all(sum, na.rm=T) %>%
  write_csv('health and cost results by year.csv')



#make plots
file_species$name -> files



read_xlsx(file.path(results_dir, 'PH Coal plants.xlsx'), sheet='All', skip=1, n_max=45) -> CFPPs


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

file_species$scenarioName <- "Operating CFPPs"
file_species$scenarioName[file_species$scenario=='oprnew_a'] <- "Operating&Proposed CFPPs"
file_species$titletxt <- NA
file_species$titletxt <- paste0(ifelse(file_species[["hr"]]<=24,paste0("Maximum ",file_species[["hr"]],"-hour "),
                                    ifelse(file_species[["type"]]=="concentration","Annual mean ","Annual total ")),
                             file_species[["speciesName"]]," ",file_species[["type"]],
                             "\nfrom ",file_species[["scenarioName"]])

queue=which(file_species$type=='concentration' & (file_species$period == 'annual' |  !is.na(file_species$threshold)))  #1:nrow(file_species) #which(file_species$scenario=='ppmine') #
test=F
fn.ext <- "" #add an extension to end of filenames to avoid overwriting

#set max value shown in graphs for each variable
colorkeybasis <- "opr_all"  #'matar1' #set NULL to set colorkey separately for each scenario


if(is.null(file_species$k)) file_species$k <- NA

if(!is.null(colorkeybasis)) {
  for(file in which(file_species$scenario==colorkeybasis &
                    1:nrow(file_species) %in% queue &
                    is.na(file_species$k))) {
    rfile <- gsub(".csv",".tif",files[file])
    raster(rfile) -> conc_R
    prb = ifelse(file_species$type[file] == 'deposition', .999625, 1)
    file_species[file_species$species == file_species[file,"species"] &
                file_species$period == file_species[file,"period"]
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

  #k=ifelse(file_species[file,"type"]=="deposition",quantile(values(conc_R),probs=.9995),max(values(conc_R)))
  if(is.null(colorkeybasis)) {
    k=quantile(values(conc_R),probs=.9995)
    file_species[file,"k"] <- k
  } else file_species[file,"k"] -> k

  thr <- file_species[file,"threshold"]
  exceed <- !is.na(thr) & max(values(conc_R)) >= thr
  if(!is.na(thr)) print(paste("threshold", ifelse(exceed,"","not"),"exceeded for",file_species[file,"titletxt"]))

  CFPP_SP -> CFPPplot

  if("png" %in% outputs) {
    plumeBreaks <- c(seq(0,1,1/40)^3,2000)
    if(file_species[file,"species"] %in% c("pm25","tpm10","tsp")) {
      colRamp <- colorRampPalette(colors=c("white","gray","yellow","red","black"))(42)
      labelcol="blue"
      wetlandcol="purple"
    } else {
      colRamp <- colorRampPalette(colors=c("white","gray","cyan","blue","purple"))(42)
      labelcol="black"
      wetlandcol="red" }

    plumeBreaks <- plumeBreaks * k

    al <- seq(0,k,sigfloor(k/5))
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
                    main=file_species[file,"titletxt"],ylab.right=file_species[file,"unit"]) +
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
      expPop[[file_species[file,"name"]]] <- data.frame(contP_UTM@data,
                                                     levelname=paste0(contP_UTM$level,file_species[file,"unit"]),
                                                     pop=raster::extract(popCP,contP_UTM,sum,na.rm=T),
                                                     area=area(contP_UTM))

    if("kml" %in% outputs) {
      spTransform(contP_UTM,CRS(proj4string(gridLL))) -> contP
      outL <- paste0(gsub("\n"," ",file_species[file,"titletxt"]),fn.ext)

      colorRampPalette(c("steelblue","yellow","orange","red","darkred"))(length(lvls)) -> yorb


      png("label.png",width=1000,height=100,pointsize=16,bg = "transparent")

      decimals <- lvls %>% subset(.>0) %>% min %>%
        log10 %>% -. %>% ceiling %>% max(0)
      legendlvls <- round(lvls, decimals)

      leg.title <- paste0(gsub(fn.ext, "", outL),
                          " (", file_species[file,"unit"], ")")

      par(mar = rep(.5, 4))
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      legend("topleft", legend = legendlvls, col=yorb, pch = 15,
             xjust=0.5, yjust=0,horiz=T,
             title = leg.title,
             bg="white",
             text.width = 1.2*max(strwidth(legendlvls[-length(legendlvls)])))
      dev.off()

      kml_open(file.name=paste0(outL,".kml"))
      kml_layer(obj=contP, subfolder.name=file_species[file,"unit"],
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
  expPop %>% ldply(.id='name') %>% left_join(file_species %>% sel(name, titletxt, threshold)) -> expPop2
  expPop2 %>% filter(min >= threshold) %>% group_by(name, titletxt, threshold) %>%
    summarise_at(c('pop', 'area'), sum, na.rm=T) -> pop_exceed

  write_csv(expPop2, paste0("expPop_new_format",fn.ext,".csv"))
  write_csv(pop_exceed, paste0("threshold_exceedances",fn.ext,".csv"))

  outF <- file(paste0("expPop",fn.ext,".csv"),"w")
  names(expPop) <- gsub("\n"," ",file_species[queue,"titletxt"])
  for(n in names(expPop)) {
    writeLines(n,outF)
    write.table(expPop[[n]],outF,append=T,row.names=F,quote=T,sep=",")
    writeLines("",outF)
  }
  close(outF)
}


#deposition totals
file_species %>%
  subset(type=='deposition') -> depodf
depodf %>%
  subset(select='name',drop=T) %>%
  gsub('.csv','.tif',.) %>% stack -> depoR
crs(depoR) <- crs(gridR)

depoR * area(depoR) * 10^2 -> depoR #hectares to km2
names(depoR) <- paste0(depodf$species,'_',depodf$scenario)

#get deposition by land use type
raster(file.path(hia_raster_dir,'ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')) -> lc
projectRaster(crop(lc,gridLL), gridR, method='ngb') -> lc.utm
read.csv(file.path(hia_raster_dir,'ESACCI-LC-Legend.csv'),sep=';',stringsAsFactors = F) -> lc.leg

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
prot %<>% spTransform(crs(gridR))s

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
names(cc) <- file_species$titletxt[queue] %>% gsub('\n', ' ', .)
cityConcs@data %<>% bind_cols(cc)

cbind(cityConcs@coords,cityConcs@data) %>% write_csv(paste0("cityConcs.csv"))

