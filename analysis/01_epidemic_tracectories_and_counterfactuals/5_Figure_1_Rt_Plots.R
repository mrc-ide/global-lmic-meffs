# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))

# Loading ECDC Data, Joining World Bank Metadata and Plotting
ecdc <- get_ecdc(date_0) %>%
  mutate(continentExp = factor(continentExp), dateRep = as.Date(dateRep)) %>%
  select(Region, countryterritoryCode, continentExp) %>%
  group_by(Region, countryterritoryCode, continentExp) %>%
  filter(row_number() == 1)

# Accessing World Bank Metadata
raw_wb_metadata <- get_brt_world_bank_classification(date_0)
wb_metadata <- raw_wb_metadata %>%
  rename(iso = ï..country_code) %>%
  dplyr::select(iso, income_group) %>%
  filter(income_group != "") %>%
  mutate(income_group = factor(income_group, levels = rev(c("Low income", "Lower middle income", "Upper middle income", "High income")))) %>%
  left_join(ecdc, by = c("iso" = "countryterritoryCode"))

# Loading ECDC Data, Joining World Bank Metadata and Plotting
ecdc <- get_ecdc(date_0) %>%
  mutate(continentExp = factor(continentExp), dateRep = as.Date(dateRep))


dirname(rstudioapi::getActiveDocumentContext()$path)

# Collating Report Dates
date_0 <- "2020-07-04"
four_param <- reports_4parameter_day(date_0)

# Extracting Output for 4 Parameter Models
x <- lapply(seq_along(four_param$id), function(y) {
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive",
                        "lmic_reports_google_pmcmc", four_param$id[y], "grid_out.rds")
  four_out <- readRDS(four_out)
  temp <- extract_Rt(four_out, four_param$country[y])
  print(y)
  return(temp)
})

rt <- do.call(rbind, x)
colnames(rt)
rt$date <- as.Date(rt$date)
rt <- rt[, c(3, 2, 1, 4, 5)] %>%
  left_join(wb_metadata, by = "iso")
# new_rt_all <- rt %>%
#   group_by(iso, rep) %>%
#   arrange(date) %>%
#  complete(date = seq.Date(min(date), max(date), by = "days"))
# column_names <- colnames(new_rt_all)[-c(1,2,3)]
# new_rt_all <- fill(new_rt_all, all_of(column_names), .direction = c("down"))
# new_rt_all <- fill(new_rt_all, all_of(column_names), .direction = c("up"))

# Country Rts
country_rt <- rt %>%
  group_by(iso, date) %>%
  summarise(Rt_min = quantile(Rt, 0.025),
            Rt_q25 = quantile(Rt, 0.25),
            Rt_q75 = quantile(Rt, 0.75),
            Rt_max = quantile(Rt, 0.975),
            Rt = mean(Rt))

# Continent Rts
continent_rt <- rt %>%
  mutate(continentExp = ifelse(continentExp == "Oceania" | continentExp == "Asia", "Asia & Oceania", as.character(continentExp))) %>%
  mutate(continentExp = ifelse(continentExp == "America", "Americas", as.character(continentExp))) %>%
  group_by(continentExp, date) %>%
  summarise(Rt_min = quantile(Rt, 0.025),
            Rt_q25 = quantile(Rt, 0.25),
            Rt_q75 = quantile(Rt, 0.75),
            Rt_max = quantile(Rt, 0.975),
            Rt = mean(Rt)) %>%
  filter(date > "2020-02-20")

colours <- c("#EA3A74", "#F3AB4C", "#545E75", "#3F826D")
alpha <- scales::alpha(colours, alpha = 0.2)

# Save as 12.5 x 4.5
ggplot(continent_rt, aes(x = date, y = Rt, ymin = Rt_min, ymax = Rt_max, fill = continentExp)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  scale_fill_manual(values = colours) +
  geom_ribbon(mapping = aes(ymin = Rt_q25, ymax = Rt_q75)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_grid(~continentExp) +
  theme(axis.text = element_text(size=12)) +
  xlab("") +
  theme(legend.position = "none")
