# package set up
library(globallmicmeffs)
date_0 <- "2020-06-17"

reports <- reports_4parameter_day(date_0)
get <- vector("list", nrow(reports))

brt <- get_brt_predictions(date_0)
for(i in seq_along(get)) {
  message(i)
  out <- readRDS(file.path(here::here(),
                           "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc/",
                           (reports$id[i]),"grid_out.rds"))
  df <- out$replicate_parameters
  if(nrow(brt[[reports$country[i]]])>0) {

    df$Rt0 <- vapply(seq_along(df$start_date),
                     function(x){
                       out$pmcmc_results$inputs$Rt_func(
                         brt[[reports$country[i]]]$C[match(df$start_date[x], brt[[reports$country[i]]]$date)],
                         df$R0[x],
                         Meff = df$Meff[x]
                       )}, numeric(1))


    df$Rt_now <- vapply(seq_along(df$start_date),
                          function(x){
                            out$pmcmc_results$inputs$Rt_func(
                              brt[[reports$country[i]]]$C[match(as.Date(date_0), brt[[reports$country[i]]]$date)],
                              df$R0[x],
                              Meff = df$Meff[x]
                            )}, numeric(1))

  } else {
    df$Rt0 <- df$R0
    df$Rt_06_16 <- df$R0
  }

  df$iso3c <- reports$country[[i]]
  df$pld <- out$interventions$date_Meff_change
  get[[i]] <- df

}
for_will <- do.call(rbind, get)

library(tidyverse)
wb <- get_brt_world_bank_classification(date_0)
for_will$continent <- countrycode::countrycode(for_will$iso3c, "iso3c", "continent")
for_will$income <- wb$income_group[match(for_will$iso3c,wb$country_code)]
for_will$income <- factor(as.character(for_will$income),levels = c( "Low income", "Lower middle income", "Upper middle income", "High income"))



ecdc <- get_ecdc(date_0)
ecdc$iso3c <- ecdc$countryterritoryCode
iso_d_10 <- ecdc %>% group_by(iso3c) %>% summarise(sum_d = sum(deaths)) %>% filter(sum_d >= 100) %>% select(iso3c) %>% unlist %>% as.character()
sum_d <- ecdc %>% group_by(iso3c) %>% summarise(sum_d = sum(deaths,na.rm=TRUE))

for_will$sum_deaths <- sum_d$sum_d[match(for_will$iso3c, sum_d$iso3c)]

library(cowplot)
ratio <- for_will  %>% filter(iso3c %in% iso_d_10) %>% ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl/Meff,y=income,fill=income)) +
  ggridges::geom_density_ridges() + geom_hline(yintercept = 1)

meff <- for_will  %>% filter(iso3c %in% iso_d_10) %>%ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff,y=income,fill=income)) +
  ggridges::geom_density_ridges() + geom_hline(yintercept = 1)

meff_pl <- for_will %>% filter(iso3c %in% iso_d_10) %>% ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl,y=income,fill=income)) +
  ggridges::geom_density_ridges() + geom_hline(yintercept = 1)

x11()
cowplot::plot_grid(cowplot::get_legend(ratio+theme(legend.position = "top")),
                   cowplot::plot_grid(ratio+theme(legend.position = "none"),
                                      meff+theme(legend.position = "none"),
                                      meff_pl+theme(legend.position = "none"),ncol=3),
                   rel_heights = c(1,10),ncol=1)

## BOXES

ratio <- for_will  %>% filter(iso3c %in% iso_d_10)  %>%
  filter(as.Date(pld) < as.Date(date_0) - 30) %>%
  ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl/Meff,y=income,fill=income)) +
  geom_boxplot(notch = TRUE) + geom_vline(xintercept = 1)

meff <- for_will  %>% filter(iso3c %in% iso_d_10)  %>%
  filter(as.Date(pld) < as.Date(date_0) - 30) %>% ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff,y=income,fill=income)) +
  geom_boxplot(notch = TRUE) + geom_vline(xintercept = 3)

meff_pl <- for_will  %>% filter(iso3c %in% iso_d_10)  %>%
  filter(as.Date(pld) < as.Date(date_0) - 30) %>%  ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl,y=income,fill=income)) +
  geom_boxplot(notch = TRUE) + geom_vline(xintercept = 3)

cowplot::plot_grid(cowplot::get_legend(ratio+theme(legend.position = "top")),
                   cowplot::plot_grid(ratio+theme(legend.position = "none"),
                                      meff+theme(legend.position = "none"),
                                      meff_pl+theme(legend.position = "none"),ncol=3),
                   rel_heights = c(1,10),ncol=1)


## POINTS

ratio <- for_will  %>% filter(iso3c %in% iso_d_10)  %>%
  filter(as.Date(pld) < as.Date(date_0) - 30) %>%
  ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl/Meff,y=income,color=continent,size=sum_deaths)) +
  geom_point(notch = TRUE,position = ) + geom_vline(xintercept = 1)

meff <- for_will  %>% filter(iso3c %in% iso_d_10)  %>%
  filter(as.Date(pld) < as.Date(date_0) - 30) %>% ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff,y=income,color=continent,size=sum_deaths)) +
  geom_point(notch = TRUE,width=0.2) + geom_vline(xintercept = 3)

meff_pl <- for_will  %>% filter(iso3c %in% iso_d_10)  %>%
  filter(as.Date(pld) < as.Date(date_0) - 30) %>%  ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl,y=income,color=continent,size=sum_deaths)) +
  geom_point(notch = TRUE,width=0.2) + geom_vline(xintercept = 3)


cowplot::plot_grid(cowplot::get_legend(ratio+theme(legend.position = "top")),
                   cowplot::plot_grid(ratio+theme(legend.position = "none"),
                                      meff+theme(legend.position = "none"),
                                      meff_pl+theme(legend.position = "none"),ncol=3),
                   rel_heights = c(1,10),ncol=1)


for_will  %>% ungroup %>% group_by(continent, iso3c, income) %>% summarise_all(mean) %>%
  ggplot(aes(x=Meff_pl/Meff,y=interaction(income,continent),fill=income)) +
  ggridges::geom_density_ridges() + geom_hline(yintercept = 1)
