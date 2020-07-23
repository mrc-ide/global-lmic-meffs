# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted); library(DBI)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))

## 1. Get Parameters from the 3 Param Fit 
date_0 <- "2020-07-04"
reports <- reports_3parameter_day(date_0)
outputs_3_param <- list()
for (i in 1:nrow(reports)) {
  three_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", "lmic_reports_google_pmcmc_no_decouple", reports$id[i], "grid_out.rds")
  three_out <- readRDS(three_out)
  median_pred_deaths_3_param <- format_output(three_out, var_select = "deaths") %>%
    group_by(compartment, t) %>%
    summarise(median = median(y, na.rm = TRUE)) %>%
    filter(!is.na(t))
  obs_deaths <- three_out$pmcmc_results$inputs$data$deaths
  missing_zeroes <- length(median_pred_deaths_3_param$median) - length(obs_deaths)
  obs_deaths <- c(rep(0, missing_zeroes), obs_deaths)
  obs_dates <- three_out$pmcmc_results$inputs$data$date
  missing_dates <- length(median_pred_deaths_3_param$median) - length(obs_dates)
  obs_dates <- c(seq.Date(from = obs_dates[1] - missing_dates, to = obs_dates[1] - 1, by = 1), obs_dates)
  median_pred_deaths_3_param$date <- obs_dates
  median_pred_deaths_3_param$obs_deaths <- obs_deaths
  median_pred_deaths_3_param$country <- three_out$parameters$country
  outputs_3_param[[i]] <- median_pred_deaths_3_param
  print(i)
}

output_3_param <- do.call(rbind, outputs_3_param)

a <- ggplot(output_3_param, aes(x = log(median + 1), y = log(obs_deaths + 1), colour = date)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  lims(y = c(0, max(log(output_4_param$obs_deaths + 1))))

## 1. Get Parameters from the 4 Param Fit 
date_0 <- "2020-07-04"
reports <- reports_4parameter_day(date_0)
outputs_4_param <- list()
for (i in 1:nrow(reports)) {
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", "lmic_reports_google_pmcmc", reports$id[i], "grid_out.rds")
  four_out <- readRDS(four_out)
  median_pred_deaths_4_param <- format_output(four_out, var_select = "deaths") %>%
    group_by(compartment, t) %>%
    summarise(median = median(y, na.rm = TRUE)) %>%
    filter(!is.na(t))
  obs_deaths <- four_out$pmcmc_results$inputs$data$deaths
  missing_zeroes <- length(median_pred_deaths_4_param$median) - length(obs_deaths)
  obs_deaths <- c(rep(0, missing_zeroes), obs_deaths)
  obs_dates <- four_out$pmcmc_results$inputs$data$date
  missing_dates <- length(median_pred_deaths_4_param$median) - length(obs_dates)
  obs_dates <- c(seq.Date(from = obs_dates[1] - missing_dates, to = obs_dates[1] - 1, by = 1), obs_dates)
  median_pred_deaths_4_param$date <- obs_dates
  median_pred_deaths_4_param$obs_deaths <- obs_deaths
  median_pred_deaths_4_param$country <- four_out$parameters$country
  outputs_4_param[[i]] <- median_pred_deaths_4_param
  print(i)
}

output_4_param <- do.call(rbind, outputs_4_param)

b <- ggplot(output_4_param, aes(x = log(median + 1), y = log(obs_deaths + 1), colour = date)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  lims(y = c(0, max(log(output_4_param$obs_deaths + 1))))

a + b +
  plot_layout(guides = 'collect')

option: options(dplyr.summarise.inform = FALSE)
calc_MAE <- function(out, date) {
  if (date == "2020-07-04") {
    median_deaths <- format_output(out, var_select = "deaths") %>%
      group_by(compartment, t) %>%
      summarise(median = median(y, na.rm = TRUE)) %>%
      filter(!is.na(t))
    obs_deaths <- out$pmcmc_results$inputs$data$deaths
    missing_zeroes <- length(median_deaths$median) - length(obs_deaths)
    obs_deaths <- c(rep(0, missing_zeroes), obs_deaths)
    obs_dates <- out$pmcmc_results$inputs$data$date
    missing_dates <- length(median_deaths$median) - length(obs_dates)
    obs_dates <- c(seq.Date(from = obs_dates[1] - missing_dates, to = obs_dates[1] - 1, by = 1), obs_dates)
    median_deaths$date <- obs_dates
    median_deaths$obs_deaths <- obs_deaths
    median_deaths$country <- out$parameters$country
    MAE <- median_deaths %>% 
      summarise(MAE = mean(abs(obs_deaths - median))) 
  } else {
    median_deaths <- out$output %>%
      group_by(compartment, t) %>%
      summarise(median = median(y, na.rm = TRUE)) %>%
      filter(!is.na(t))
    obs_deaths <- out$pmcmc_results$inputs$data$deaths
    missing_zeroes <- length(median_deaths$median) - length(obs_deaths)
    obs_deaths <- c(rep(0, missing_zeroes), obs_deaths)
    obs_dates <- out$pmcmc_results$inputs$data$date
    missing_dates <- length(median_deaths$median) - length(obs_dates)
    obs_dates <- c(seq.Date(from = obs_dates[1] - missing_dates, to = obs_dates[1] - 1, by = 1), obs_dates)
    median_deaths$date <- obs_dates
    median_deaths$obs_deaths <- obs_deaths
    median_deaths$country <- out$parameters$country
    MAE <- median_deaths %>% 
      summarise(MAE = mean(abs(obs_deaths - median))) 
  }
  return(MAE$MAE)
}

all_reports <- reports_all()
out <- readRDS(file.path(here::here(), "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc/", all_reports$id[1], "grid_out.rds"))
calc_MAE(out, all_reports$date[1])

out <- readRDS(file.path(here::here(), "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc/", all_reports$id[13], "grid_out.rds"))
calc_MAE(out, all_reports$date[13])


MAEs <- pbapply::pblapply(seq_along(all_reports$id), function(x) {
  
  message(x)
  
  if(all_reports$model[x] == "3p") {
    out <- readRDS(
      file.path(here::here(),
                "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc_no_decouple/",
                all_reports$id[x],
                "grid_out.rds")
    )
    model <- "3_param"
  } else {
    out <- readRDS(
      file.path(here::here(),
                "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc/",
                all_reports$id[x],
                "grid_out.rds")
    )
    model <- "4_param"
  }
  
  if(all_reports$date[x] == "2020-07-04") {
    MAE <- calc_MAE(out, all_reports$date[x])
  } else {
    MAE <- calc_MAE(out, all_reports$date[x])
  }
  return(MAE)
})

all_reports$MAE <- unlist(MAEs)
raw_wb_metadata <- get_brt_world_bank_classification(date_0)
all_reports$income <- raw_wb_metadata$income_group[match(all_reports$country, raw_wb_metadata$ï..country_code)]

overall_MAEs <- all_reports %>%
  group_by(date, model) %>%
  summarise(mean_MAE = mean(MAE)) %>%
  spread(model, mean_MAE)

overall_MAEs_income <- all_reports %>%
  group_by(date, model, income) %>%
  summarise(mean_MAE = mean(MAE)) %>%
  spread(model, mean_MAE)

overall_MAEs_income <- all_reports %>%
  group_by(date, model, income) %>%
  summarise(mean_MAE = mean(MAE))

ggplot(overall_MAEs_income, aes(x = as.factor(date), y = mean_MAE, col = model)) +
  geom_point() +
  facet_wrap(~income, scale = "free_y")

ggplot(all_reports, aes(x=as.factor(date), y = log(MAE), fill = model)) +
  geom_boxplot() +
  facet_wrap(~income)

ggplot(all_reports, aes(x=as.factor(date), y = MAE, col = model)) +
  geom_boxplot() +
  facet_wrap(~income, scales = "free_y")

# Plotting Map
date_0 <- "2020-07-04"
reports <- reports_4parameter_day(date_0)
raw_uncouple_values <- c()
total_deaths <- c()
for (i in 1:nrow(reports)) {
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", "lmic_reports_google_pmcmc", reports$id[i], "grid_out.rds")
  four_out <- readRDS(four_out)
  chain_1 <- tail(four_out$pmcmc_results$chains$chain1$results, 5000)
  chain_2 <- tail(four_out$pmcmc_results$chains$chain2$results, 5000)
  chain_3 <- tail(four_out$pmcmc_results$chains$chain3$results, 5000)
  chain <- rbind(chain_1, chain_2, chain_3)
  Meff <- mean(chain$Meff)
  Meff_pl <- mean(chain$Meff_pl)
  uncouple <- Meff - Meff_pl
  raw_uncouple_values[i] <- uncouple
  total_deaths[i] <- sum(four_out$pmcmc_results$inputs$data$deaths)
  print(i)
}
uncouple_values <- data.frame(country = reports$country, total_deaths = total_deaths,
                              uncouple = raw_uncouple_values)
uncouple_values <- uncouple_values[uncouple_values$total_deaths >= 50, ]
uncouple_values <- uncouple_values %>%
  mutate(indicator = case_when(uncouple <= 0 ~ "green", 
                               uncouple <= 1 & uncouple > 0 ~ "orange",
                               TRUE ~ "red"))
world <- ne_countries(scale = "medium", returnclass = "sf")
new_world <- world %>%
  left_join(uncouple_values, by = c("iso_a3" = "country")) %>%
  mutate(indicator = factor(indicator))

ggplot(data = new_world) +
  geom_sf(aes(fill = indicator)) +
  scale_fill_manual(values = c("#54C225", "orange", "red"), na.value = "light grey") +
  coord_sf(ylim = c(-55, 80)) +
  theme_bw() +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 




