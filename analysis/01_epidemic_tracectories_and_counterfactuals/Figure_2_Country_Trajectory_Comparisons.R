# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted); library(DBI)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))

# Collating Report Dates
date_0 <- "2020-06-27"
three_param <- reports_3parameter_day(date_0)
four_param <- reports_4parameter_day(date_0)

# Defining Countries for Representative Examples
countries_of_interest <- c("PHL", "NGA", "SAU", "PAN", "IDN", "ARG", "COL", "CHE", "CZE", "PER", "PRT")
indices <- which(four_param$country %in% countries_of_interest)

# Loading Google Mobility Data for Countries of Interest
mobility <- get_brt_predictions(date_0)
mobility <- do.call(rbind, mobility)
mobility <- mobility %>%
  filter(date < "2020-06-30") %>%
  filter(iso3c %in% countries_of_interest)

# Extracting Data for Countries of Interest
three_param_list <- list()
four_param_list <- list()
counter <- 1
for (i in indices) {
  
  iso <- four_param$country[i]
  
  # Loading the 3 Param Results
  three_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", 
                         "lmic_reports_google_pmcmc_no_decouple", three_param$id[i], "grid_out.rds")
  three_param_list[[iso]] <- readRDS(three_out)
  
  # Loading the 4 Param Results
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", 
                        "lmic_reports_google_pmcmc", four_param$id[i], "grid_out.rds")
  four_param_list[[iso]] <- readRDS(four_out)
  
  counter <- counter + 1
}

process_output <- function(output, iso) {
  x <- output[[iso]]
  country_specific_mob <- mobility %>%
    filter(iso3c == iso)
  start_dates <- data.frame(start_date = x$replicate_parameters$start_date, 
                            replicate = seq(1, 200, 1))
  deaths_data <- x$pmcmc_results$inputs$data
  
  y <- format_output(x, var_select = "deaths") %>%
    filter(!is.na(t), !is.na(y)) %>%
    left_join(start_dates, by = "replicate") %>%
    mutate(replicate = factor(replicate)) %>%
    group_by(replicate) %>%
    mutate(min_date = min(t)) %>%
    mutate(rel_date = t - min_date) %>%
    mutate(actual_date = start_date + rel_date) %>%
    select(replicate, actual_date, y) %>%
    group_by(actual_date) %>%
    summarise(deaths_min = quantile(y, 0.05),
              deaths_max = quantile(y, 0.95),
              deaths_mean = mean(y)) %>%
    left_join(deaths_data, by = c("actual_date" = "date")) %>%
    left_join(country_specific_mob, by = c("actual_date" = "date"))
  return(list(model_output = y,
              mobility_data = country_specific_mob))
}

mob_death_plot <- function(output, colour) {
  y <- output$model_output
  mob <- output$mobility_data
  a <- ggplot(y, aes(x = actual_date, y = deaths_mean, ymin = deaths_min, ymax = deaths_max)) +
    geom_ribbon(alpha = 0.2, fill = colour) +
    geom_point(aes(x = actual_date, y = deaths), col = "black") +
    geom_line(col = colour, size = 1.5) +
    theme(axis.title.x = element_blank()) +
    labs(y = "Daily Deaths", title = paste0("Output for ", output$mobility_data$iso3c[1]))
  b <- ggplot(mob, aes(x = date, y = C)) +
    geom_line(col = colour, size = 1.5) +
    theme(axis.title.y = element_text(vjust = 2)) +
    labs(y = "Mobility Relative to Baseline") +
    lims(y = c(0, 1.2))
  a/b
  return(list(a = a, b = b))
}

# Countries
countries_of_interest <- c("PAN", "IDN", "ARG", "COL", "CHE", "CZE", "PER", "PRT")

# Meff_pl ~ 0
overall <- process_output(four_param_list, "PAN")
a <- mob_death_plot(overall, "red")
overall <- process_output(four_param_list, "IND")
a <- mob_death_plot(overall, "red")

# 0 < Meff_pl < Meff
overall <- process_output(four_param_list, "PHL")
b <- mob_death_plot(overall, "orange")
overall <- process_output(four_param_list, "COL")
mob_death_plot(overall, "orange")

# Meff_pl ~ Meff
overall <- process_output(four_param_list, "CHE")
mob_death_plot(overall, "dark green")
overall <- process_output(four_param_list, "CZE")
mob_death_plot(overall, "dark green")

# Meff_pl > Meff
overall <- process_output(four_param_list, "PRT")
c <- mob_death_plot(overall, "dark green")


(a$a + b$a + c$a) / (a$b + b$b + c$b)
