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

# Boosted Regression Tree Inference of Mobility Plotting
mobility <- get_brt_predictions(date_0)
mobility <- do.call(rbind, mobility)

observed_mobility <- mobility %>%
  filter(observed == TRUE)
a <- ggplot(observed_mobility, aes(x = 100 * (C - 1), y = 100 * (C_predict - 1), col = income_group)) +
  geom_point(size = 3) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#C6E9F2", "#A5D6EF", "#5C97BC", "#29547A")) +
  theme_bw() +
  ylim(c(-95, 15)) +
  xlim(c(-95, 25)) +
  labs(x = "Observed Mobility Change (%)", y = "Predicted Mobility Change (%)", colour = "") +
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -2, size = 12),
        axis.title.y = element_text(vjust = +4, size = 12), 
        legend.text = element_text(size = 12), axis.text = element_text(size = 11),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")) +
  guides(col=guide_legend(ncol=2)) +
  geom_abline(intercept = 0, slope = 1, color = "black", 
              linetype = "dashed", size = 1)


specific_countries <- observed_mobility %>%
  filter(iso3c == "COL" | iso3c == "IND" | iso3c == "PHL" | iso3c == "ZWE") %>%
  mutate(iso3c = factor(iso3c))
country_labels <- c("Colombia", "India", "Phillipines", "Zimbabwe")
names(country_labels) <- c("COL", "IND", "PHL", "ZWE")

b <- ggplot(specific_countries, aes(x = date, y = C)) +
  geom_point(size = 2) +
  facet_wrap(~iso3c, labeller = labeller(country_labels)) +
  geom_line(aes(x = date, y = C_predict, col = iso3c), size = 2) +
  scale_colour_manual(values = c("#DBC453", "#DD954D", "#BD7EE2", "#63AD4A")) +
  theme_bw() +
  labs(x = "Date", y = "Mobility Change (%)") +
  theme(legend.position = "none")



plot(mobility$CAF$C, mobility$AGO$C_predict, ylim = c(0, 1.2), xlim = c(0, 1.2))

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




# Collating Reports for the 3 and 4 Parameter Models 
date_0 <- "2020-06-27"
three_param <- reports_3parameter_day(date_0)
four_param <- reports_4parameter_day(date_0)

