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
countries_of_interest <- c("BRA", "NGA", "PAN")
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
  
  # Loading the 3 Param Results
  three_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", 
                         "lmic_reports_google_pmcmc_no_decouple", three_param$id[i], "grid_out.rds")
  three_param_list[[counter]] <- readRDS(three_out)
  
  # Loading the 4 Param Results
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", 
                        "lmic_reports_google_pmcmc", four_param$id[i], "grid_out.rds")
  four_param_list[[counter]] <- readRDS(four_out)
  
  counter <- counter + 1
}


i <- 2
# Graphs for 3 Param Fit
x <- three_param_list[[i]]
deaths_data <- x$pmcmc_results$inputs$data
start_dates <- data.frame(start_date = x$replicate_parameters$start_date, replicate = seq(1, 200, 1))
country_specific_mob <- mobility %>%
  filter(iso3c == countries_of_interest[i])
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

scaling_factor <- 100
scale <- 1/scaling_factor
a <- ggplot(y, aes(x = actual_date, y = deaths_mean, ymin = deaths_min, ymax = deaths_max)) +
  geom_ribbon(alpha = 0.2, fill = "#F85E00") +
  geom_line(col = "#F85E00", size = 1.5) +
  geom_line(aes(x = actual_date, y = scaling_factor * C), size = 1.5) +
  geom_point(aes(x = actual_date, y = deaths), col = "#F85E00") +
  scale_y_continuous(name = "Deaths", sec.axis = sec_axis(~.*scale, name = "Mobility Relative to Baseline")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title.y = element_text(vjust = 2),
        axis.title.y.right = element_text(vjust = 3))

# Graphs for 4 Param Fit
x <- four_param_list[[i]]
start_dates <- data.frame(start_date = x$replicate_parameters$start_date, replicate = seq(1, 200, 1))

y <- format_output(x, var_select = "deaths") %>%
  filter(!is.na(t), !is.na(y)) %>%
  left_join(start_dates, by = "replicate") %>%
  mutate(replicate = factor(replicate)) %>%
  group_by(replicate) %>%
  mutate(min_date = min(t)) %>%
  mutate(rel_date = t - min_date) %>%
  mutate(actual_date = start_date + rel_date) %>%
  select(t, replicate, actual_date, y) %>%
  group_by(actual_date) %>%
  summarise(deaths_min = quantile(y, 0.05),
            deaths_max = quantile(y, 0.95),
            deaths_mean = mean(y)) %>%
  left_join(deaths_data, by = c("actual_date" = "date")) %>%
  left_join(country_specific_mob, by = c("actual_date" = "date"))

four_scaling_factor <- 50
four_scale <- 1/four_scaling_factor
b <- ggplot(y, aes(x = actual_date, y = deaths_mean, ymin = deaths_min, ymax = deaths_max)) +
  geom_ribbon(alpha = 0.2, fill = "#119822") +
  geom_line(col = "#119822", size = 1.5) +
  geom_line(aes(x = actual_date, y = four_scaling_factor * C), size = 1.5) +
  geom_point(aes(x = actual_date, y = deaths), col = "#119822") +
  scale_y_continuous(name = "Deaths", sec.axis = sec_axis(~.*four_scale, name = "Mobility Relative to Baseline")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title.y = element_text(vjust = 2),
        axis.title.y.right = element_text(vjust = 3))

a + b



