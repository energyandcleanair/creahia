#read HIA input data
if(!exists('crfs')) { read_csv(paste0(HIApath, 'CRFs.csv')) -> crfs
} else warning('crfs exists, using existing data')
read_csv(paste0(HIApath, 'epi_for_hia_C40.csv')) -> epi
read_csv(paste0(HIApath, 'GDP.csv')) -> gdp
read_csv(paste0(HIApath, 'valuation.csv')) -> vals


#fill in default values for places missing data
fillcol <- function(df2, targetcols) {
  for(coln in targetcols) {
    df2[[coln]] %>% median(na.rm=T) -> defval
    df2[[coln]] %<>% na.fill(defval)
  }
  return(df2)
}

adddefos <- function(df, exl='pop') {
  names(df)[sapply(df, is.numeric) & names(df) %notin% exl] -> targetcols
  df %>% ddply(.(estimate, Region, IncomeGroup), fillcol, targetcols) %>% 
    ddply(.(estimate), fillcol, targetcols)
}

epi %<>% adddefos

names(crfs) %<>% gsub('RR_', '', .)
crfs$Exposure %<>% gsub('PM2\\.5', "PM25", .)
crfs$Incidence %<>% gsub('AllCauses', "AllCause", .)
crfs$effectname <- paste0(crfs$Incidence %>% gsub('\\.per|_base', '', .), 
                          '_', 
                          crfs$Exposure %>% gsub('\\..*|nrt', '', .))

calc_causes = c(paste0(c('NCD.LRI', 'LRI.child'), '_YLLs'),
                paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
                paste0(c(names(causes), 'LRI.child', 'Diabetes'), '_Deaths')) %>% 
  unique

#add missing admin regions
epi %<>% filter(ISO3=='CHN') %>% mutate(ISO3='HKG', pop=7.392e6, country='Hong Kong', IncomeGroup="High income") %>% bind_rows(epi)
epi %<>% filter(ISO3=='CHN') %>% mutate(ISO3='MAC', pop=622567, country='Macau', IncomeGroup="High income") %>% bind_rows(epi) %>% distinct

countriesLow$ADM0_A3[countriesLow$SOV_A3 == 'GB1'] %>% as.character -> UK
names(UK) <- rep('GBR', length(UK))
merge_into <- c(IND='KAS', AUS='IOA', UK)
use_as_proxy <- c(CHN='HKG', CHN='MAC', AUT='LIE', VUT='PLW', ITA='SMR')

country.recode <- function(ISOcodes, replacements=merge_into) {
  for(i in 1:length(replacements))
    ISOcodes[ISOcodes == replacements[i]] <- names(replacements)[i]
  return(ISOcodes)
}
