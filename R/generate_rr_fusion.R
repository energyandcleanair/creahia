generate_rr_fusion <- function(){

  # Both code source and excel files were provided by Burnett
  # We use code source, but compare with excel for validation
  # CIs for burden analyses (e.g. small perturbations)
  # However, values generated aren't exactly matching yet

  rrs_excel <- generate_rrs_from_excel()
  rrs_code <- generate_rr_fusion_from_code()

  # Visual validation
  # bind_rows(
  #   rrs_excel %>% mutate(source='excel'),
  #   rrs_source %>% mutate(source='code')) %>%
  #   filter(exposure < 100) %>%
  #   ggplot(aes(x=exposure, y=central, color=source)) +
  #   geom_line() +
  #   facet_grid(age~cause)

  # We remove CV because pipeline not validated yet (i.e. ensure there's no double counting)
  rrs <- rrs_code %>%
    filter(cause != "CV")

  return(rrs)
}


#' This one reads directly from the lookup table provided by Burnett
#'
#' @param cause
#' @param age
#' @param params
#' @param datatheta
#' @param col_indexes
#' @param z_max
#' @param z_step
#' @param synthetize
#'
#' @returns
#' @export
#'
#' @examples
generate_rrs_from_excel <- function(){

  filepath <- get_hia_path("fusion/Fusion model RR lookup table.xlsx")

  rr_copd_lc_lri_diabetes <- readxl::read_xlsx(filepath,
                    range="Fusion RR!B21:N3021",
                    col_names = c(
                      "exposure",
                      "COPD_central_25+", "COPD_low_25+", "COPD_high_25+",
                      "LC_central_25+", "LC_low_25+", "LC_high_25+",
                      "LRI_central_25+", "LRI_low_25+", "LRI_high_25+",
                      "Diabetes_central_25+", "Diabetes_low_25+", "Diabetes_high_25+"
                    ))


  ages <- c("25-29",
            "30-34",
            "35-39",
            "40-44",
            "45-49",
            "50-54",
            "55-59",
            "60-64",
            "65-69",
            "70-74",
            "75-79",
            "80-84",
            "85-89",
            "90-94",
            "95+")

  estimates <- c("central", "low", "high")

  # cross them
  age_estimates <- expand.grid(estimates, ages) %>%
    mutate(colnames=paste0(Var1, "_", Var2)) %>%
    pull(colnames)

  rr_ihd <- readxl::read_xlsx(filepath,
                              range="Fusion RR!P21:BI3021",
                              col_names = c("exposure", paste0("IHD_", age_estimates)))

  rr_stroke <- readxl::read_xlsx(filepath,
                              range="Fusion RR!BK21:DD3021",
                              col_names = c("exposure", paste0("Stroke_", age_estimates)))


  # We don't take CV, NCD+LRI, NCD+Stroke, NCD+IHD and CVM for now as they aren't age specific
  # and may have some overlap

  rrs <- left_join(
    rr_copd_lc_lri_diabetes,
    rr_ihd) %>%
    left_join(
      rr_stroke
    ) %>%
    mutate(exposure=as.numeric(exposure)) %>%
    pivot_longer(
      cols = -exposure,
      names_to = c("cause", "estimate", "age"),
      names_pattern = "(.*)_(.*)_(.*)"
    ) %>%
    spread(estimate, value)

  return(rrs)
}


#' Takes file sent by Burnett and turns it into a compatible file
#' Not exactly matching excel file
generate_rr_fusion_from_code <- function(){

  params <- read.csv(get_hia_path("fusion/Fusion Parameters Jul 7, 2021.csv"), header = T, check.names = F)
  col_indexes <- get_col_indexes(params)
  datatheta <- get_datatheta()

  cause_age <- col_indexes %>%
    distinct(cause, age) %>%
    inner_join(
      datatheta %>%
        distinct(cause, age)
    )

  rr_all <- cause_age %>%
    pmap_dfr(function(cause, age){
      generate_rrs_per_cause_age(cause=cause,
                                 age=age,
                                 params=params,
                                 datatheta=datatheta,
                                 col_indexes=col_indexes,
                                 synthetize=TRUE) %>%
        as_tibble(rownames = "exposure") %>%
        mutate(
          cause = cause,
          exposure = as.numeric(exposure),
          age=age)
    }) %>%
    bind_rows()

  # Filter causes to match those of original IER for now
  rr <- rr_all %>%
    mutate(cause=recode_gbd_causes(cause, stop_on_unknown = FALSE)) %>%
    mutate(age=recode_age(age)) %>%
    filter(!is.na(age))

  # Convert LRI to LRIChild when Under 5
  rr <- rr %>%
    mutate(cause=case_when(cause == CAUSE_LRI &
                             age == AGE_CHILDREN ~ CAUSE_LRICHILD,
                           TRUE ~ cause))

  # Format
  rr <- rr %>%
    select(cause,
           exposure,
           age,
           central=rr_mean,
           low=rr_lower,
           high=rr_upper)

  return(rr)
}

#' Generate 1000 RRs for a given cause, age and different pollution levels.
#'
#' If synthetize is TRUE, it will return the mean and 95% CI of the 1000 RRs.
#'
#' Synthetizing is losing infos that could be used in z1 to z2 RR2s and only produces
#' confidence intervals that work for 0 to z
#'
#' @param cause
#' @param age
#' @param params
#' @param datatheta
#' @param col_indexes
#' @param z_max
#' @param z_step
#' @param synthetize
#'
#' @return matrix of n_exposure x 1000 RR estimates or n_exposure x 3 (central, lower, upper)
#' @export
#'
#' @examples
generate_rrs_per_cause_age <- function(cause,
                                       age,
                                       params,
                                       datatheta,
                                       col_indexes,
                                       z_max=300,
                                       z_step=0.1,
                                       synthetize=FALSE
                                       ){

  #R code to calculate Fusion RR based on input parameters
  i_gamma <- col_indexes %>%
    filter(cause == !!cause, age == !!age, variable == "gamma") %>%
    pull(col_index)
  i_mu <- col_indexes %>%
    filter(cause == !!cause, age == !!age, variable == "mu") %>%
    pull(col_index)
  i_rho <- col_indexes %>%
    filter(cause == !!cause, age == !!age, variable == "rho") %>%
    pull(col_index)

  stopifnot(all(c(length(i_gamma), length(i_mu), length(i_rho)) == 1))


  gamma <- as.numeric(params[2:1001, i_gamma])
  mu <- as.numeric(params[2:1001, i_mu])
  rho <- as.numeric(params[2:1001, i_rho])


  row_theta <- datatheta %>%
    filter(cause == !!cause, age == !!age) %>%
    pull(row_index)


  theta=as.numeric(datatheta[row_theta,5])
  betaLL=as.numeric(datatheta[row_theta,3])
  lambda=(theta-mu)/(theta*(1-rho))


  #calculate Fusion RR (FUS) without cf

  # Define sequence of concentrations from 0 to T by 0.1 units
  xind=seq(0, theta, z_step)

  # Calculate integral
  nsim=length(gamma)
  # G=matrix(0, nsim, length(xind))
  # for (j in 1:nsim){
  #   G[j,]=1/(1+((1-rho[j])/rho[j])*(ifelse(xind<mu[j], 0, xind- mu[j])/(theta- mu[j]))^lambda[j])
  # }

  xind_matrix <- matrix(rep(xind, each=nsim), nrow=nsim)
  mu_matrix <- matrix(rep(mu, times=length(xind)), nrow=nsim)
  condition <- xind_matrix < mu_matrix
  numerator <- ifelse(condition, 0, xind_matrix - mu_matrix)
  G <- 1/(1 + ((1-rho)/rho) * (numerator/(theta-mu))^lambda)

  # INT=matrix(0, nsim, length(xind))
  # for (j in 1:nsim) {
  #   for (k in 1:length(xind)){
  #     INT[j,k]=0.1*(sum(G[j,1:k])-G[j,1])
  #   }}

  # Vectorized calculation of cumulative sums
  # Calculate cumulative sum for each row of G, subtract first element, and multiply by 0.1
  INT <- 0.1 * (t(apply(G, 1, cumsum)) - G[,1])

  #define sequence of concentrations from T to max concentration
  xup= seq(theta + z_step, z_max, z_step)
  xxx=c(xind[1:dim(INT)[2]], xup)
  endind=dim(INT)[2]

  #calculate Fusion RR nsim times
  nsim=1000
  len=length(xxx)

  # FUS=matrix(0, nsim, len)
  # for (j in 1:nsim) {
  #   for (k in 1:len) {
  #     if (xxx[k]<theta) {FUS[j,k]=gamma[j]*(INT[j,k])}
  #     if (xxx[k]>=theta) {FUS[j,k]=gamma[j]*(INT[j,endind]+theta*log(max(xxx[k],theta)/theta)*rho[j])}
  #   }}
  FUS=matrix(0, nsim, len)

  # Create condition matrices
  xxx_matrix <- matrix(rep(xxx, each=nsim), nrow=nsim)
  below_theta <- xxx_matrix < theta

  # Split the calculation based on the position relative to theta
  theta_index <- which.min(abs(xxx - theta))  # Find index closest to thetay

  # Calculate both conditions at once
  FUS[, 1:theta_index] <- gamma * INT  # For xxx < theta
  gamma_matrix <- matrix(gamma, nrow=nsim, ncol=len-theta_index)
  rho_matrix <- matrix(rho, nrow=nsim, ncol=len-theta_index)
  int_endind_matrix <- matrix(INT[, endind], nrow=nsim, ncol=len-theta_index)
  log_terms <- matrix(log(xxx[(theta_index+1):len]/theta),
                      nrow=nsim,
                      ncol=len-theta_index,
                      byrow=TRUE)

  FUS[, (theta_index+1):len] <- gamma_matrix * (int_endind_matrix + theta * log_terms * rho_matrix)

  eFUS = exp(FUS)

  # meanFUS=matrix(0, length(xxx), 1)
  # lclFUS=matrix(0, length(xxx), 1)
  # uclFUS=matrix(0, length(xxx), 1)

  # for (k in 1:length(xxx)) {
  #   meanFUS[k]=exp(mean(FUS[,k]))
  #   lclFUS[k]=exp(quantile(FUS[,k], 0.025))
  #   uclFUS[k]= exp(quantile(FUS[,k], 0.975))
  # }

  # Vectorized calculation of means and confidence intervals
  # meanFUS <- exp(apply(FUS, 2, mean))
  # lclFUS <- exp(apply(FUS, 2, quantile, probs=0.025))
  # uclFUS <- exp(apply(FUS, 2, quantile, probs=0.975))

  # Return a matrix n_conc x n_estimate x n_sims
  rrs <- t(eFUS) %>%
    `rownames<-`(xxx)

  if(synthetize){
    rr_mean <- apply(rrs, 1, mean)
    rr_lower <- apply(rrs, 1, quantile, probs=0.025)
    rr_upper <- apply(rrs, 1, quantile, probs=0.975)
    rrs <- cbind(rr_mean, rr_lower, rr_upper) %>%
      `rownames<-`(xxx)
  }

  return(rrs)
}





get_datatheta <- function(){

  read.csv(get_hia_path("fusion/LL Fusion Parameters theta eq 97.5.csv"), header = T) %>%
    rename(cause=COD,
           age=Age) %>%
    # turn empty to NA
    mutate(cause=case_when(cause == "" ~ NA_character_,
                           TRUE ~ cause)) %>%
    fill(cause, theta) %>%
    mutate(row_index=row_number()) %>%
    # replave >age with age+
    mutate(age=case_when(str_detect(age, ">") ~ paste0(gsub(">", "", age), "+"),
                         TRUE ~ age)) %>%
    # # NCD&LRI to NCD.LRI
    # mutate(cause=case_when(cause == "NCD&LRI" ~ "NCD.LRI",
    #                        TRUE ~ cause)) %>%
    rowwise() %>%
    # Split <5 & 25+ into two rows
    mutate(age = case_when(str_detect(age, "<5 & 25+") ~ list(c("0-4", "25+")),
                           TRUE ~ list(age))) %>%
    unnest(age)

}





get_rr_ratio <- function(cause,
                         age,
                         params,
                         datatheta,
                         col_indexes,
                         z_baseline,
                         z_perm
){

  z_max <- 300 # Still needs to be higher than theta in current version
  z_step <- 0.1

  rrs <- generate_rrs_per_cause_age(cause, age, params, datatheta, col_indexes, z_max, z_step)

  i_baseline <- which(as.numeric(rownames(rrs)) == round(z_baseline,1))
  i_perm <- which(as.numeric(rownames(rrs)) == round(z_perm,1))

  rr_ratio <- rrs[i_perm, ] / rrs[i_baseline, ]


  data.frame(
    central = mean(rr_ratio),
    lower = unname(quantile(rr_ratio, 0.025)),
    upper = unname(quantile(rr_ratio, 0.975))
  )
}




#' Redurn column indexes for parameter gamma, mu, row per cause and age in params
#'
#' @param params
#'
#' @return
#' @export
#'
#' @examples
get_col_indexes <- function(params){

  tibble(colname=names(params)) %>%
    # Get cause
    mutate(
      # Remove trailing dots and "AGE"
      cause_tmp = str_remove(colname, "[>|<|&|0-9|Age| ]+$"),
      # replace ST with Stroke
      cause_tmp=case_when(cause_tmp == "ST" ~ "Stroke",
                          grepl("Lung", cause_tmp) ~ "Lung Cancer",
                          grepl("Type.II", cause_tmp) ~ "Diabetes",
                          cause_tmp=="" ~ NA_character_,
                          TRUE ~ cause_tmp)
    ) %>%
    mutate(cause=cause_tmp) %>%

    fill(cause, .direction = "down") %>%
    # Get age

    mutate(
      # Only digits or dots
      age = case_when(is.na(cause_tmp) ~ NA_character_,
                      str_detect(colname, "<5 & Age >25") ~ "0-4##25+",
                      T ~ stringr::str_extract(gsub(" ","",colname), "[0-9|>|<]+")),
      # Replace ..age with age+
      age = case_when(str_detect(age, "^>") ~ paste0(gsub(">", "", age), "+"),
                      TRUE ~ age)
    ) %>%
    # Split age using ## and unnest
    fill(age) %>%
    mutate(variable=rep(c("gamma", "mu", "rho"), times=ncol(params)/3)) %>%
    mutate(col_index=row_number()) %>%
    rowwise() %>%
    mutate(age = str_split(age, "##")) %>%
    unnest(age)
}

