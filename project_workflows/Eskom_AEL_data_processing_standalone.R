require(tidyverse); require(magrittr); require(readxl); require(lubridate); require(creahelpers)

project_dir="G:/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"

emissions_dir <- file.path(project_dir,"emissions")

infile=file.path(emissions_dir, 'Eskom-AEL-data.xlsx')

excel_sheets(infile) %>% subset(!grepl('Summary|GEM', .)) -> plantnames

plantnames %>%
  pbapply::pblapply(function(plant_to_read) {
    message(plant_to_read)
    silent_read(infile, sheet=plant_to_read, skip=3, n_max=4, col_names = F) %>% t %>%
      as_tibble() %>% set_names(.[1,]) %>% '['(-1,T) %>%
      rename(unit=starts_with('Units'), MW=contains('MW'), stack=contains('Stack'), AQCS=contains('Tech')) %>%
      mutate(across(MW, as.numeric),
             plant=plant_to_read) %>%
      fill(stack, AQCS, .direction='down') ->
      plant_desc

    silent_read(infile, sheet=plant_to_read, skip=7, col_names = F, n_max=4) -> header
    header %>% t %>% as_tibble() -> header_df

    header_df[8,2, drop=T] -> plant_desc$plant_MW
    header_df[6:8,2] <- header_df[6:8,3]
    header_df[6:8,3] <- NA
    header_df[6:7,3] <- header_df[6:7,4]
    header_df[6:7,4] <- NA
    header_df[4:5,4] <- NA

    header_df %>%
      fill(everything(), .direction='down') %>%
      apply(1, function(x) x %>% na.omit %>% head(4) %>%
              matrix(nrow=1) %>% as_tibble) %>%
      bind_rows() %>%
      mutate(col=names(header)) ->
      vardata

    silent_read(infile, sheet=plant_to_read, skip=11, col_names = F) -> alldata
    max(which(!is.na(alldata[[1]]))) -> datarows
    silent_read(infile, sheet=plant_to_read, skip=11, col_names = F, n_max=datarows) -> alldata

    names(alldata)[1:2] <- vardata$V1[1:2]
    alldata %>%
      mutate(across(-Month, as.numeric)) %>%
      select(-...9) %>% #remove empty column
      pivot_longer(c(-Month, -Days), names_to = 'col') %>%
      left_join(vardata) %>%
      mutate(plant=plant_to_read) -> data

    return(list(plant_description=plant_desc,
                operating_data=data))
  }) -> d

d %>% lapply('[[', 'plant_description') %>%
  bind_rows() %>%
  group_by(plant, stack, plant_MW) %>%
  summarise(MW=sum(MW),
            units=unit %>% paste(collapse='; '),
            AQCS=AQCS %>% na.omit %>% unique  %>% paste(collapse='; ')) ->
  plantdata


d %<>% lapply('[[', 'operating_data') %>%
  bind_rows() %>%
  mutate(V2=recode(V2, SOx='SO2'))



d %>% filter(plant=='Kusile', V2=='SO2') %>% unite(name, V3, V4) %>% select(-col) %>% pivot_wider() %>% copy.xl








