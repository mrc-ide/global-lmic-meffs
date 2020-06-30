# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))


as.Date(out$replicate_parameters$start_date[y]) == out$interventions$date_R0_change

# Defining Additional Rt Extraction Function Here
extract_Rt <- function(out, replicates, iso) {
  rts <- lapply(1:replicates, function(y) {
    tt <- squire:::intervention_dates_for_odin(dates = out$interventions$date_R0_change,
                                               change = out$interventions$R0_change,
                                               start_date = out$replicate_parameters$start_date[y],
                                               steps_per_day = 1/out$parameters$dt)
    
    Rt <- squire:::evaluate_Rt_pmcmc(
      R0_change = out$interventions$R0_change[out$interventions$date_R0_change>=out$replicate_parameters$start_date[y]], 
      R0 = out$replicate_parameters$R0[y], 
      Meff = out$replicate_parameters$Meff[y], 
      Meff_pl = out$replicate_parameters$Meff_pl[y],
      date_R0_change = out$interventions$date_R0_change[out$interventions$date_R0_change>=out$replicate_parameters$start_date[y]],
      date_Meff_change = out$interventions$date_Meff_change, 
      roll = out$pmcmc_results$inputs$roll,
      start_date = out$replicate_parameters$start_date[y]) 
    
    df <- data.frame(
      "Rt" = Rt,
      "Meff" = out$replicate_parameters$Meff[y],
      "date" = c(as.Date(out$replicate_parameters$start_date[y]), as.Date(out$replicate_parameters$start_date[y]) + round((tt$tt*(out$parameters$dt)))),
      "iso" = iso,
      rep = y,
      stringsAsFactors = FALSE)
    df$pos <- seq_len(nrow(df))
    return(df)
  })
  rt <- do.call(rbind, rts)
  return(rt)
}

# Collating Report Dates
date_0 <- "2020-06-27"
three_param <- reports_3parameter_day(date_0)
four_param <- reports_4parameter_day(date_0)

# Extracting Output for 4 Parameter Models
four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", "lmic_reports_google_pmcmc", four_param$id[10], "grid_out.rds")
four_out <- readRDS(four_out)



x <- extract_Rt(four_out, 200, four_param$country[10])



rt <- do.call(rbind, rts
  
}


# Extracting Rt Results by Country
rt <- pbapply::pblapply(seq_along(reports$id), function(x) {
  iso <- reports$country[x]
  out <- file.path(here::here(), "analysis/data/raw_data/server_results/", "archive", "lmic_reports_google", reports$id[x], "grid_out.rds")
  out <- readRDS(out)
  
)