# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))

# Boosted Regression Tree Inference of Mobility Plotting
date_0 <- "2020-07-04"
mobility <- get_brt_predictions(date_0)
mobility <- do.call(rbind, mobility)

# First BRT Panel - Observed vs BRT Predicted Mobility
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


# Second BRT Panel - Plotting Predictions and Observed for 4 Representative Countries
specific_countries <- observed_mobility %>%
  filter(iso3c == "COL" | iso3c == "IND" | iso3c == "PHL" | iso3c == "ZWE") %>%
  mutate(iso3c = factor(iso3c))
country_labels <- c("Colombia", "India", "Phillipines", "Zimbabwe")
names(country_labels) <- c("COL", "IND", "PHL", "ZWE")

b <- ggplot(specific_countries, aes(x = date, y = C)) +
  geom_point(size = 2) +
  facet_wrap(~iso3c, labeller = labeller(country_labels)) +
  geom_line(aes(x = date, y = C_predict, col = iso3c), size = 2) +
  scale_colour_manual(values = c("#EBA4A4", "#BB9BBF", "#6C7F81", "#8EB7CC")) +
  theme_bw() +
  labs(x = "Date", y = "Mobility Change (%)") +
  theme(legend.position = "none")

a + b


