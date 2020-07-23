# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))

# Collating Report Dates
date_0 <- "2020-07-04"
reports <- reports_3parameter_day(date_0)

# Accessing World Bank Metadata
raw_wb_metadata <- get_brt_world_bank_classification(date_0)
wb_metadata <- raw_wb_metadata %>%
  rename(iso = ï..country_code) %>%
  dplyr::select(iso, income_group) %>%
  filter(income_group != "") %>%
  mutate(income_group = factor(income_group, levels = rev(c("Low income", "Lower middle income", "Upper middle income", "High income"))))

# Loading ECDC Data, Joining World Bank Metadata and Plotting
ecdc <- get_ecdc(date_0) %>%
  mutate(continentExp = factor(continentExp), dateRep = as.Date(dateRep))
ecdc_deaths <- ecdc %>%
  left_join(wb_metadata, by = c("countryterritoryCode" = "iso")) %>%
  mutate(continent = countrycode::countrycode(countryterritoryCode, "iso3c", destination = "continent"))

ecdc_deaths_income <- ecdc_deaths %>%
  filter(!is.na(income_group)) %>%
  ungroup() %>%
  group_by(dateRep, income_group) %>%
  summarise(deaths = sum(deaths))

# ------------------------------------------------------------------------
# Plotting Cumulative Deaths by Income Strata
# ------------------------------------------------------------------------

a <- ggplot(ecdc_deaths_income, aes(x = dateRep, y = deaths, fill = income_group)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Daily Deaths") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "blue"),
                    name = "Income Strata",
                    breaks = c("High income", "Upper middle income", "Lower middle income", "Low income"),
                    labels = c("High Income", "Upper-Middle Income", "Lower Middle Income", "Low Income")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none")

# Alternative - Plotting by Continent
ecdc_deaths_continent <- ecdc_deaths %>%
  filter(!is.na(continent)) %>%
  ungroup() %>%
  group_by(dateRep, continent) %>%
  summarise(deaths = sum(deaths))

alt <- ggplot(ecdc_deaths_continent, aes(x = dateRep, y = deaths, fill = continent)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Daily Deaths") +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none")


# ------------------------------------------------------------------------
# Plotting R0 on the World Map
# ------------------------------------------------------------------------

# Plotting R0 on the World Map
grids <- out_3parameter_list(date_0)
param_sums <- lapply(seq_along(grids), function(i) {
  x <- grids[[i]]
  pars <- x$replicate_parameters
  pars$start_date <- as.numeric(pars$start_date)
  y <- summarise_all(pars, "mean")
  df <- data.frame("var" = names(y),
                   "y" = as.numeric(y))
  df$iso <- names(grids)[i]
  return(df)
})
params <- do.call(rbind, param_sums)
R0 <- params %>%
  filter(var == "R0")

map_data_and_shape <- read.csv(file.path(here::here(),"analysis/data/map_plot_data/start_R0_Meff_date.csv"), stringsAsFactors = FALSE) %>%
  select(long, lat, group, GID_0) %>%
  left_join(R0, by = c("GID_0" = "iso"))

#Plotting R0 start
b <- ggplot() +
  geom_polygon(data = map_data_and_shape,
               aes(x = long, y = lat, group = group, fill = y), color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis(option = "C") +
  labs(fill = "Starting R0",
       ylab = "",
       xlab = "") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = "none")

# ------------------------------------------------------------------------
# Plotting Start Date on the World Map
# ------------------------------------------------------------------------

# Plotting R0 on the World Map
grids <- out_3parameter_list(date_0)
param_sums <- lapply(seq_along(grids), function(i) {
  x <- grids[[i]]
  pars <- x$replicate_parameters
  pars$start_date <- as.numeric(pars$start_date)
  y <- summarise_all(pars, "mean")
  df <- data.frame("var" = names(y),
                   "y" = as.numeric(y))
  df$iso <- names(grids)[i]
  return(df)
})
params <- do.call(rbind, param_sums)
R0 <- params %>%
  filter(var == "R0")

map_data_and_shape <- read.csv("analysis/data/map_plot_data/start_R0_Meff_date.csv", stringsAsFactors = FALSE) %>%
  select(long, lat, group, GID_0) %>%
  left_join(R0, by = c("GID_0" = "iso"))

#Plotting R0 start
b <- ggplot() +
  geom_polygon(data = map_data_and_shape,
               aes(x = long, y = lat, group = group, fill = y), color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis(option = "C") +
  labs(fill = "Starting R0",
       ylab = "",
       xlab = "") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = "none")


# Plotting Mobility to Date
mobility <- get_brt_predictions(date_0)
mobility <- do.call(rbind, mobility)
mobility <- mobility %>%
  filter(date < "2020-06-30")
mobility$continent <- countrycode::countrycode(mobility$iso, "iso3c", destination = "continent")

mobility_continent_summary <- mobility %>%
  group_by(date, continent) %>%
  summarise(C = mean(C), C_predict = mean(C_predict)) %>%
  mutate(iso3c = continent)

mobility_income_group_summary <- mobility %>%
  group_by(date, income_group) %>%
  summarise(C = mean(C), C_predict = mean(C_predict)) %>%
  mutate(iso3c = case_when(income_group == "Low income" ~ "LIC",
                           income_group == "Lower middle income" ~ "LMIC",
                           income_group == "Upper middle income" ~ "UMIC",
                           income_group == "High income" ~ "HIC"))


c <- ggplot(mobility, aes(x = date, y = C_predict, group = iso3c)) +
  geom_line(col = "grey", alpha = 0.2) +
  geom_line(data = mobility_continent_summary, aes(x = date, y = C_predict, colour = continent), size = 1.5) +
  theme_bw() +
  labs(x = "Date", y = "Change in Mobility Relative to Baseline") +
  lims(y = c(0.2, 1.1)) +
  scale_x_date(limits = as.Date(c("2020-02-14","2020-06-01"))) +
  theme(legend.position = "none")

min_mobility <- mobility %>%
  group_by(iso3c) %>%
  summarise(min_mob = min(C_predict), income_group = unique(income_group), continent = unique(continent))

e <- ggplot(min_mobility, aes(x = continent, y = min_mob, colour = continent)) +
  geom_boxplot(fill = NA, size = 1.5) +
  geom_jitter(data = min_mobility, aes(x = continent, y = min_mob, fill = continent),
              pch = 21, size = 3, width = 0.2) +
  theme_bw() +
  theme(legend.position = "none")

# Extracting Distribution of R0s

R0s <-lapply(seq_along(grids), function(x) {
  data.frame("R0" = grids[[x]]$replicate_parameters$R0, "iso3c" = names(grids)[x])
  })
R0s <- do.call(rbind, R0s)
R0s$continent <- countrycode::countrycode(R0s$iso3c, "iso3c", destination = "continent")
ggplot(R0s, aes(x=R0, fill = continent)) + geom_density(alpha=0.5)

# Extracting Rt Results by Country
rt <- pbapply::pblapply(seq_along(reports$id), function(x) {

  iso <- reports$country[x]
  out <- file.path(here::here(),
                   "analysis/data/raw_data/server_results/",
                   "archive", "lmic_reports_google_pmcmc",
                   reports$id[x], "grid_out.rds")
  out <- readRDS(out)

  rt <- extract_Rt(out, iso)

  return(rt)
})

names(rt) <- reports$country
rt_all <- do.call(rbind, rt)
rt_all$date <- as.Date(rt_all$date)
rt_all <- rt_all[, c(4, 3, 2, 1, 5, 6)]

new_rt_all <- rt_all %>%
  group_by(iso, rep) %>%
  arrange(date) %>%
  complete(date = seq.Date(min(rt_all$date), max(rt_all$date), by = "days"))

column_names <- colnames(new_rt_all)[-c(1,2,3)]
new_rt_all <- fill(new_rt_all, column_names, .direction = c("down"))
new_rt_all <- fill(new_rt_all, column_names, .direction = c("up"))
new_rt_all$continent <- countrycode::countrycode(new_rt_all$iso, "iso3c", destination = "continent")

countries_Rt <- new_rt_all %>% group_by(iso, date) %>%
  summarise(Rt = median(Rt))
countries_Rt$continent <- countrycode::countrycode(countries_Rt$iso, "iso3c", destination = "continent")

continents_Rt <- new_rt_all %>% group_by(continent, date) %>%
  summarise(Rt = median(Rt)) %>%
  mutate(iso = continent)

d <- ggplot(countries_Rt, aes(x=date, y = Rt, group = iso)) +
  geom_line(lwd = 1, colour = "grey", alpha = 0.15) +
  theme_bw() +
  geom_line(data = continents_Rt, aes(x = date, y = Rt, colour = continent), size = 1.5) +
  lims(y = c(0, 4)) +
  scale_x_date(limits = as.Date(c("2020-02-14","2020-06-01"))) +
  theme(legend.position = "none")

countries_Meff <- new_rt_all %>% group_by(iso) %>%
  summarise(Meff = median(Meff))
countries_Meff$continent <- countrycode::countrycode(countries_Meff$iso, "iso3c", destination = "continent")

e <- ggplot(countries_Meff, aes(x = continent, y = Meff, colour = continent)) +
  geom_boxplot(fill = NA, size = 1.5) +
  geom_jitter(data = countries_Meff, aes(x = continent, y = Meff, fill = continent),
              pch = 21, size = 3, width = 0.2) +
  theme_bw() +
  theme(legend.position = "none")


# Getting Model Outputs from Our Runs Fitted to Empirical Data
grids <- out_3parameter_list(date_0)
countries <- names(grids)
actual_country_outputs <- vector(mode = "list", length = length(countries))
for (i in 1:length(countries)) {
  country <- countries[i]
  x <- grids[[country]]
  y <- format_output(x, var_select = "deaths") %>%
    filter(!is.na(y)) %>%
    group_by(t) %>%
    summarise(deaths_025 = quantile(y, 0.025),
              mean_deaths = mean(y),
              deaths_975 = quantile(y, 0.975))
  y$country <- country
  actual_country_outputs[[i]] <- y
}
actual_country_outputs <- do.call(rbind, actual_country_outputs)
actual_country_outputs$continent <- countrycode::countrycode(actual_country_outputs$country, "iso3c", destination = "continent")

summary_actual_continent_outputs <- actual_country_outputs %>%
  group_by(t, continent) %>%
  summarise(deaths_025 = sum(deaths_025),
            mean_deaths = sum(mean_deaths),
            deaths_975 = sum(deaths_975))
summary_actual_continent_outputs$scenario <- "actual"

ggplot(summary_actual_continent_outputs, aes(x = t, y = mean_deaths, col = continent)) +
  geom_line() +
  facet_wrap(~continent, scales = "free_y")

# Running Counterfactual Runs Based on the Fitted Baseline R0s and Start Dates
unmit_country_outputs <- vector(mode = "list", length = length(countries))
for (i in 1:length(countries)) {
  country <- countries[i]
  x <- grids[[country]]
  y <- unmit_sim(x, "2020-06-01")
  z <- squire::format_output(y, "deaths") %>%
    filter(!is.na(y)) %>%
    group_by(t) %>%
    summarise(deaths_025 = quantile(y, 0.025),
              mean_deaths = mean(y),
              deaths_975 = quantile(y, 0.975))
  z$country <- country
  unmit_country_outputs[[i]] <- z
}
unmit_country_outputs <- do.call(rbind, unmit_country_outputs)
unmit_country_outputs$continent <- countrycode::countrycode(unmit_country_outputs$country, "iso3c", destination = "continent")
saveRDS(unmit_country_outputs, file.path(here::here(),"analysis/data/derived_data/01_unmitigated_epidemics.rds"))


summary_unmit_continent_outputs <- unmit_country_outputs %>%
  group_by(t, continent) %>%
  summarise(deaths_025 = sum(deaths_025),
            mean_deaths = sum(mean_deaths),
            deaths_975 = sum(deaths_975))
summary_unmit_continent_outputs$scenario <- "unmit"

ggplot(summary_unmit_continent_outputs, aes(x = t, y = mean_deaths, col = continent)) +
  geom_line() +
  facet_wrap(~continent, scales = "free_y")

overall <- rbind(summary_actual_continent_outputs, summary_unmit_continent_outputs)

e <- ggplot(overall, aes(x = t, y = mean_deaths, col = scenario)) +
  geom_line() + scale_y_log10() +
  facet_wrap(~continent, scales = "free_y")


layout <- "
BBBDDDDD
BBBDDDDD
EEEEEEEE
EEEEEEEE
EEEEEEEE
"
d + e + b +
  plot_layout(design = layout)

layout <- "
BBBDDDDD
BBBDDDDD
CCCDDDDD
CCCDDDDD
EEEEEEEE
EEEEEEEE
EEEEEEEE
EEEEEEEE
EEEEEEEE
"
c + d + e + b +
  plot_layout(design = layout)





# cont_plot <- ggplot(conts, aes(x=date, y = Rt, ymin = Rt_q25, ymax = Rt_q75,
#                                group = continent, color = continent, fill = continent)) +
#   geom_line(lwd=2) +
#   geom_point(data = conts %>% filter(date == min(date))) +
#   geom_ribbon(alpha = 0.2, colour = NA) +
#   theme_bw() +
#   facet_wrap(~continent) +
#   geom_hline(yintercept = 1, linetype = "dashed") +
#   theme(legend.position = "none") +
#   xlab("Date") +
#   theme(axis.text = element_text(size=14)) +
#   scale_colour_manual(values = c("#43BCCD", "#F86624", "#662E9B", "#19AD45"),
#                       labels = c("Africa", "Latin America", "Asia", "Europe"),
#                       name = "Continent") +
#   scale_fill_manual(values = c("#43BCCD", "#F86624", "#662E9B", "#19AD45"),
#                     labels = c("Africa", "Latin America", "Asia", "Europe"),
#                     name = "Continent") +
#   scale_x_date(limits = as.Date(c("2020-02-14","2020-06-01")))
#
# facet_wrap(~continent) +
#   geom_hline(yintercept = 1, linetype = "dashed") +
#   theme(legend.position = "none") +
#   xlab("Date") +
#   theme(axis.text = element_text(size=14)) +
#   scale_colour_manual(values = c("#43BCCD", "#F86624", "#662E9B", "#19AD45"),
#                       labels = c("Africa", "Latin America", "Asia", "Europe"),
#                       name = "Continent") +
#   scale_fill_manual(values = c("#43BCCD", "#F86624", "#662E9B", "#19AD45"),
#                     labels = c("Africa", "Latin America", "Asia", "Europe"),
#                     name = "Continent") +

# Plotting Deaths Per Million Pre and Post Lockdown
# base_path <- here::here()
# file_path <- "/analysis/data/intervention_data/lockdown_timing_ACAPs_derived.csv"
#
# lockdown_timing <- read.csv(paste0(base_path, file_path))
# lockdown_timing <- lockdown_timing %>%
#   select(Code, Income_group, Date_suppression) %>%
#   filter(!is.na(Date_suppression)) %>%
#   mutate(Code = as.character(Code)) %>%
#   mutate(Date_suppression = as.Date(Date_suppression, format = c("%d/%m/%Y")))
# lockdown_timing$Date_suppression
#
# not_in_ECDC <- which(!(lockdown_timing$Code %in% ecdc$countryterritoryCode))
# lockdown_timing <- lockdown_timing[-not_in_ECDC, ]
# deaths_pre_post <- matrix(nrow = nrow(lockdown_timing), ncol = 2)
# for (i in 1:nrow(lockdown_timing)) {
#   ecdc_subset <- ecdc[ecdc$countryterritoryCode == lockdown_timing$Code[i] & !is.na(ecdc$countryterritoryCode), ]
#   date_lockdown <- lockdown_timing$Date_suppression[i]
#   ecdc_pre <- ecdc_subset[ecdc_subset$dateRep < date_lockdown, ]
#   ecdc_post <- ecdc_subset[ecdc_subset$dateRep >= date_lockdown, ]
#
#   total_pre <- sum(ecdc_pre$deaths, na.rm = TRUE)
#   deaths_pre_post[i, 1] <- total_pre
#   total_post <- sum(ecdc_post$deaths, na.rm = TRUE)
#   deaths_pre_post[i, 2] <- total_post
# }
#
# plot(log(deaths_pre_post[, 1]), log(deaths_pre_post[, 2]))
