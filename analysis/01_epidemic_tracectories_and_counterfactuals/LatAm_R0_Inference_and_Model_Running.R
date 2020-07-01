# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted); library(DBI); library(squire)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Loading In 4 Parameter Fits
date_0 <- "2020-06-27"
four_param <- reports_4parameter_day(date_0)

# Country Codes
countries_of_interest <- c("ARG", "BRA", "CUB", "COL", "CHL", "ECU", "HND", "MEX", "PER")
indices <- which(four_param$country %in% countries_of_interest)

# Extracting Data for Countries of Interest
four_param_list <- list()
counter <- 1
mean_R0s <- c()
for (i in indices) {
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", 
                        "lmic_reports_google_pmcmc", four_param$id[i], "grid_out.rds")
  temp <- readRDS(four_out)
  temp_chains <- rbind(temp$pmcmc_results$chains$chain1$results[5000:10001, ],
                       temp$pmcmc_results$chains$chain2$results[5000:10001, ],
                       temp$pmcmc_results$chains$chain3$results[5000:10001, ])
  temp_R0 <- mean(temp_chains$R0, na.rm = TRUE)
  mean_R0s <- c(mean_R0s, temp_R0)
}

# Running the Model for Each Country for this R0 and Getting the Attack Rates Out
attack_rates <- matrix(nrow = 9, ncol = length(countries_of_interest))

for (i in 1:length(mean_R0s)) {
  
  temp_R0 <- mean_R0s[i]
  iso <- countries_of_interest[i]
  country <- countrycode::countrycode(iso, "iso3c", "country.name")
  x <- run_explicit_SEEIR_model(country = country, 
                                R0 = temp_R0, 
                                day_return = TRUE, 
                                replicates = 25)
  infections <- format_output(x, var_select = "infections", reduce_age = FALSE)
  population <- x$parameters$population
  average_infections_age <- infections %>%
    group_by(replicate, age_group) %>%
    summarise(total_infections = sum(y)) %>%
    group_by(age_group) %>%
    summarise(total_infections = median(total_infections))

  average_infections_age <- average_infections_age %>%
    mutate(age_cat = case_when(age_group == 1 | age_group == 2 ~ "0-10",
                               age_group == 3 | age_group == 4 ~ "10-20",
                               age_group == 5 | age_group == 6 ~ "20-30",
                               age_group == 7 | age_group == 8 ~ "30-40",
                               age_group == 9 | age_group == 10 ~ "40-50",
                               age_group == 11 | age_group == 12 ~ "50-60",
                               age_group == 13 | age_group == 14 ~ "60-70",
                               age_group == 15 | age_group == 16 ~ "70-80",
                               age_group == 17 ~ "80+")) %>%
    group_by(age_cat) %>%
    summarise(total_infections = sum(total_infections))
  attack_rates[, i] <- average_infections_age$total_infections
}

colnames(attack_rates) <- countries_of_interest
row.names(attack_rates) <- unique(average_infections_age$age_cat)
write.csv(attack_rates, file = "C:/Users/cw1716/Documents/Updated_LatAm_Infections.csv")

