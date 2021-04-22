require(tidyverse)
require(magrittr)
require(readxl)
require(plyr)

origwd <- getwd()
setwd(HIApath)

#read GEMM function fit parameters
infile <- 'GEMM Calculator (PNAS)_ab.xlsx'
read_xlsx(infile, sheet='GEMM fit parameters', skip=8, n_max=14) -> gemm.china
read_xlsx(infile, sheet='GEMM fit parameters', skip=29, n_max=14) -> gemm.exchina

bind_rows(gemm.china %>% mutate(region='inc_China'),
          gemm.exchina %>% mutate(region='ex_China')) -> gemm

#eliminate empty rows and columns
gemm[rowSums(!is.na(gemm))>1, colSums(!is.na(gemm))>0] -> gemm
write.csv(gemm, 'gemm fit parameters.csv')

#read names of causes of death
read_xlsx(infile, sheet='GEMM fit parameters', skip=6, n_max=1,
          col_names=F) %>%
  unlist %>% subset(!is.na(.)) ->
  causes

#define short names
names(causes) <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

#remove duplicated age columns
names(gemm)[1] <- 'age'
gemm %<>% sel(-contains('Age'), age)

#give parameter columns names; t=theta, se=standard error of theta, a=alpha, u=mu, p=pi
names(causes) %>% sapply(paste, c('t', 'se', 'a', 'u', 'p'), sep='_') %>%
  as.vector() -> newnames
names(gemm)[seq_along(newnames)] <- newnames

gemm %<>% gather(cause, value, -region, -age) %>%
  separate(cause, c('cause', 'param'), '_')
gemm$age[gemm$age=='30-35'] <- '30-34'

#read GBD RR values
read.csv('ier_computed_table.csv', stringsAsFactors = F) %>% sel(-X, -age) %>% 
  dplyr::filter(cause %in% c('lri', 't2_dm')) %>% 
  rename(cause_short = cause, central=rr_mean, low=rr_lower, high=rr_upper) %>% 
  mutate(cause_short = recode(cause_short, lri='LRI.child', t2_dm='Diabetes')) %>% 
  dplyr::filter(exposure <=300) -> GBD
GBD.causes <- GBD$cause_short %>% unique

#define a function to calculate the hazard ratio for a specific concentration, cause and age group
hr <- function(pm, .age='25+', .cause='NCD.LRI', .region='inc_China') {
  if(.cause %in% GBD.causes) {
    GBD %>% dplyr::filter(cause_short==.cause) -> hr.all
    hr.all %>% sel(low, central, high) %>% 
      apply(2, function(y) approx(x=hr.all$exposure, y, xout=pm)$y)
  } else {
    gemm %>% dplyr::filter(age == .age, cause == .cause, region==.region) %>%
      spread(param, value) -> p
    z = pmax(0, pm-2.4, na.rm=T)
    g = log(1+z/p$a) / (1 + exp((p$u-z)/p$p))
    se = c(-p$se, 0, p$se)
    (matrix(g) %*% (p$t + 2 * se)) %>% exp -> hr.out
    colnames(hr.out) <- c('low', 'central', 'high')
    return(hr.out)
  }
}


#read IHME mortality and morbidity data to enable country calculations
ihme <- read.csv('IHME-GBD_2017_DATA.csv') %>% dplyr::filter(metric_name == 'Number') %>% 
  gather_ihme

ihme %<>% mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric)
ihme$age_low[ihme$age_name=='Under 5'] <- 0
ihme$age_low[ihme$age_name=='All Ages'] <- -1
ihme$age <- ihme$age_name %>% gsub(' to ', '-', .) %>% gsub(' plus', '+', .)

ihme %<>% 
  dplyr::filter(age_low>=25) %>% 
  group_by_at(vars(-val, -starts_with('age'))) %>% 
  summarize_at('val', sum) %>% 
  mutate(age='25+') %>% bind_rows(ihme) %>% ungroup

ihme %<>% addiso

ihme$cause_short <- NA
ihme$cause_short[grep('Diab', ihme$cause_name)] <- 'Diabetes'
ihme$cause_short[grep('Stroke', ihme$cause_name)] <- 'Stroke'
ihme$cause_short[grep('Lower resp', ihme$cause_name)] <- 'LRI'
ihme$cause_short[grep('Non-comm', ihme$cause_name)] <- 'NCD'
ihme$cause_short[grep('Isch', ihme$cause_name)] <- 'IHD'
ihme$cause_short[grep('obstr', ihme$cause_name)] <- 'COPD'
ihme$cause_short[grep('lung canc', ihme$cause_name)] <- 'LC'


ihme %<>% 
  dplyr::filter(grepl('Lower resp|Non-comm', cause_name)) %>% 
  group_by_at(vars(-val, -starts_with('cause'))) %>% 
  summarize_at('val', sum) %>% 
  mutate(cause_name='NCD+LRI', cause_short='NCD.LRI') %>% bind_rows(ihme) %>% ungroup

calc_causes = c(paste0(c('NCD.LRI', 'LRI.child'), '_YLLs'),
                paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
                paste0(c(names(causes), 'LRI.child', 'Diabetes'), '_Deaths')) %>% 
  unique

ihme$age[ihme$age_low>=25] %>% subset(!is.na(.)) %>% unique -> adult.ages

ihme %<>% dplyr::filter(ISO3 == 'ALB') %>% mutate(ISO3='XKX', country='Kosovo') %>% bind_rows(ihme)

age.specific=c('NCD.LRI', 'Stroke', 'IHD')
orderrows = function(df) { 
  df %>% apply(1, sort) %>% t -> rr.out
  colnames(rr.out) <- c('low', 'central', 'high')
  rr.out
}

#total fossil fuel PAF for a permutation run
country.paf.perm <- function(pm.base,
                             pm.perm,
                             pop,
                             cy, cause, measure, 
                             .region="inc_China",
                             .mode = 'change') { #change or attribution?
  if(cause %in% age.specific) {
    ages = adult.ages
    w = ihme %>% 
      dplyr::filter(ISO3 == cy, cause_short==cause, measure_name == measure, 
             age %in% ages, estimate=='central')
  } else {
    w=data.frame(val=1)
    if(grepl('child', cause)) { ages='Under 5'
    } else { ages='25+' }
  }
  
  ages %>% sapply(function(.a) hr(pm.base, .cause=cause, .age=.a, .region=.region), simplify = 'array') -> rr.base
  
  if(.mode == 'change') {
    ages %>% sapply(function(.a) hr(pm.perm, .cause=cause, .age=.a, .region=.region), simplify = 'array') -> rr.perm
    paf.perm <- rr.perm / rr.base - 1
    
  } else { paf.perm = (1 - (1 / rr.base)) * (1 - pm.perm / pm.base) }
  
  if(length(dim(paf.perm))==2) { 
    paf.perm %>% t %>% 
      orderrows %>% 
      apply(2, weighted.mean, w$val) #in case the hr function didn't return an array
  } else {
    paf.perm %>% apply(1:2, weighted.mean, w$val) %>% 
      orderrows %>% apply(2, weighted.mean, w=pop) 
  }
}


country.paf <- function(pm, pop, cy, cs, ms, .region="inc_China") {
  if(grepl('child', cs)) {
    ages='Under 5'
    w=data.frame(val=1)
  } else {
    ages = adult.ages
    w = ihme %>% dplyr::filter(ISO3 == cy, cause_short==cs, measure_name == ms, age %in% ages, estimate=='central')
  }
  
  ages %>% sapply(function(.a) hr(pm, .cause=cs, .age=.a, .region=.region), simplify = 'array') -> rr
  paf <- 1 - 1 / rr
  
  if(length(dim(paf))==2) { 
    paf %>% t %>% apply(2, weighted.mean, w$val) #in case the hr function didn't return an array
  } else {
    paf %>% apply(1:2, weighted.mean, w$val, na.rm=T) %>% apply(2, weighted.mean, w=pop, na.rm=T) 
  }
}

setwd(origwd)