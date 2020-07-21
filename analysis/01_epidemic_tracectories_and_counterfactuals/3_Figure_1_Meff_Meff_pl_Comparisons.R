# Loading Required Libraries
devtools::load_all()
library(globallmicmeffs); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted); library(DBI)
conflict_prefer("select", "dplyr"); conflict_prefer("filter", "dplyr"); conflict_prefer("area", "patchwork")

# Sourcing Required Functions
source(file.path(here::here(),"analysis/01_epidemic_tracectories_and_counterfactuals/functions.R"))

# Sourcing Additional Functions
calc_DIC <- function(out, model) {

  # Combining Chains Together
  chain_1 <- tail(out$pmcmc_results$chains$chain1$results, 5000)
  chain_2 <- tail(out$pmcmc_results$chains$chain2$results, 5000)
  chain_3 <- tail(out$pmcmc_results$chains$chain3$results, 5000)
  chain <- rbind(chain_1, chain_2, chain_3)

  if (!(model %in% c("3_param", "4_param"))) {
    stop("wrong model specification")
  }

  if (model == "3_param") {

    # Calculating D_bar (posterior mean of the deviance)
    D_bar <- mean(-2 * chain$log_posterior)

    # Calculating D_hat (posterior deviance of the mean parameters)
    mean_start_date <- round(mean(chain$start_date))
    mean_R0 <- mean(chain$R0)
    mean_Meff <- mean(chain$Meff)
    mean_Meff_pl <- mean(chain$Meff_pl) # remove this once OJ's fixed

    # Calculating Prior for Mean Parameters
    pars <- c(mean_start_date, mean_R0, mean_Meff, mean_Meff_pl) # remove mean_Meff_pl once OJ's fixed
    names(pars) <- c("start_date", "R0", "Meff", "Meff_pl")
    prior <- out$pmcmc_results$inputs$prior(c(pars))

    # Calculating Likelihood for Mean Parameters
    data <- out$pmcmc_results$inputs$data
    mean_pars <- list(start_date = squire:::offset_to_start_date(data$date[1], mean_start_date),
                      R0 = mean_R0,
                      Meff = mean_Meff,
                      Meff_pl = mean_Meff_pl)

    loglik <- squire:::calc_loglikelihood(pars = mean_pars,
                                          data = out$pmcmc_results$inputs$data,
                                          squire_model = squire:::deterministic_model(),
                                          model_params = out$pmcmc_results$inputs$model_params,
                                          pars_obs = out$pmcmc_results$inputs$pars_obs,
                                          n_particles = out$pmcmc_results$inputs$n_particles,
                                          forecast_days = 0,
                                          return = "ll",
                                          Rt_args = out$pmcmc_results$inputs$Rt_args,
                                          interventions = out$interventions)
    loglik <- loglik$log_likelihood

    # Calculating D_hat = deviance of mean parameters
    D_hat <- -2 * (loglik + prior)

    # Calculating the DIC
    pD <- D_bar - D_hat
    DIC <- D_bar + pD
    return(DIC)

  } else if (model == "4_param") {

    # Calculating D_bar (posterior mean of the deviance)
    D_bar <- mean(-2 * chain$log_posterior)

    # Calculating D_hat (posterior deviance of the mean parameters)
    mean_start_date <- round(mean(chain$start_date))
    mean_R0 <- mean(chain$R0)
    mean_Meff <- mean(chain$Meff)
    mean_Meff_pl <- mean(chain$Meff_pl)

    # Calculating Prior for Mean Parameters
    pars <- c(mean_start_date, mean_R0, mean_Meff, mean_Meff_pl)
    names(pars) <- c("start_date", "R0", "Meff", "Meff_pl")
    prior <- out$pmcmc_results$inputs$prior(c(pars))

    # Calculating Likelihood for Mean Parameters
    data <- out$pmcmc_results$inputs$data
    mean_pars <- list(start_date = squire:::offset_to_start_date(data$date[1], mean_start_date),
                      R0 = mean_R0,
                      Meff = mean_Meff,
                      Meff_pl = mean_Meff_pl)

    loglik <- squire:::calc_loglikelihood(pars = mean_pars,
                                          data = out$pmcmc_results$inputs$data,
                                          squire_model = squire:::deterministic_model(),
                                          model_params = out$pmcmc_results$inputs$model_params,
                                          pars_obs = out$pmcmc_results$inputs$pars_obs,
                                          n_particles = out$pmcmc_results$inputs$n_particles,
                                          forecast_days = 0,
                                          return = "ll",
                                          Rt_args = out$pmcmc_results$inputs$Rt_args,
                                          interventions = out$interventions)
    loglik <- loglik$log_likelihood

    # Calculating D_hat = deviance of mean parameters
    D_hat <- -2 * (loglik + prior)

    # Calculating DIC = D_bar + pD
    pD <- D_bar - D_hat
    DIC <- D_bar + pD
    return(DIC)

  }
}

# Collating Report Dates
date_0 <- "2020-07-04"
three_param <- reports_3parameter_day(date_0)
four_param <- reports_4parameter_day(date_0)

DICs <- matrix(nrow = nrow(three_param), ncol = 2)
colnames(DICs) <- c("three_param", "four_param")
for (i in 1:nrow(three_param)) {
  three_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive",
                         "lmic_reports_google_pmcmc_no_decouple", three_param$id[i], "grid_out.rds")
  three_out <- readRDS(three_out)
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive",
                        "lmic_reports_google_pmcmc", four_param$id[i], "grid_out.rds")
  four_out <- readRDS(four_out)
  DICs[i, 1] <- calc_DIC(three_out, "3_param")
  DICs[i, 2] <- calc_DIC(four_out, "4_param")
  print(i)
}

diff <- DICs[, 1] - DICs[, 2]

raw_wb_metadata <- get_brt_world_bank_classification(date_0)
wb_metadata <- raw_wb_metadata %>%
  rename(iso = ï..country_code) %>%
  dplyr::select(iso, income_group) %>%
  filter(income_group != "") %>%
  mutate(income_group = factor(income_group, levels = rev(c("Low income", "Lower middle income", "Upper middle income", "High income")))) %>%
  left_join(three_param, by = c("iso" = "country")) %>%
  select(iso, income_group)


three_param <- three_param %>%
  left_join(wb_metadata, by = c("country" = "iso"))

HIC_pref <- length(three_param$country[which(diff > 5 & three_param$income_group == "High income")])
HIC_nah <- length(three_param$country[which(diff < 5 & three_param$income_group == "High income")])
HIC_pref/(HIC_pref + HIC_nah)

UMIC_pref <- length(three_param$country[which(diff > 5 & three_param$income_group == "Upper middle income")])
UMIC_nah <- length(three_param$country[which(diff < 5 & three_param$income_group == "Upper middle income")])
UMIC_pref/(UMIC_pref + UMIC_nah)

LMIC_pref <- length(three_param$country[which(diff > 5 & three_param$income_group == "Lower middle income")])
LMIC_nah <- length(three_param$country[which(diff < 5 & three_param$income_group == "Lower middle income")])
LMIC_pref/(LMIC_pref + LMIC_nah)

LIC_pref <- length(three_param$country[which(diff > 5 & three_param$income_group == "Low income")])
LIC_nah <- length(three_param$country[which(diff < 5 & three_param$income_group == "Low income")])
LIC_pref/(LIC_pref + LIC_nah)



calc_DIC(four_out, "4_param")

chain_1 <- three_out$pmcmc_results$chains$chain1$results[5000:10000, ]
chain_2 <- three_out$pmcmc_results$chains$chain2$results[5000:10000, ]
chain_3 <- three_out$pmcmc_results$chains$chain3$results[5000:10000, ]
three_overall <- rbind(chain_1, chain_2, chain_3)

chain_1 <- four_out$pmcmc_results$chains$chain1$results[5000:10000, ]
chain_2 <- four_out$pmcmc_results$chains$chain2$results[5000:10000, ]
chain_3 <- four_out$pmcmc_results$chains$chain3$results[5000:10000, ]
four_overall <- rbind(chain_1, chain_2, chain_3)

hist(three_overall$Meff, breaks = 10, xlim = c(1, 5))
hist(four_overall$Meff, breaks = 10, xlim = c(1, 5))

hist(four_overall$Meff)
hist(four_overall$Meff_pl)

## DIC over time

all_reports <- reports_all()

dics <- pbapply::pblapply(seq_along(all_reports$id), function(x) {

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
  DIC <- calc_DIC(out, model)
  return(DIC)
})

all_reports$DIC <- unlist(dics)
raw_wb_metadata <- get_brt_world_bank_classification(date_0)
all_reports$income <- raw_wb_metadata$income_group[match(all_reports$country, raw_wb_metadata$country_code)]

x11()
all_dics <- ggplot(all_reports, aes(x=as.Date(date), y = DIC, color = model)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y") +
  scale_x_date(limits=c(as.Date("2020-05-01"),as.Date("2020-07-04")), date_breaks = "1 month", date_labels = "%b %d")

income_dics <- all_reports %>% group_by(date, income, model) %>%
  summarise(y = sum(DIC)) %>%
  mutate(income = factor(income, levels = c("High income","Upper middle income","Lower middle income","Low income"))) %>%
  mutate(model = c("No uncopuling", "Uncoupling")[match(model, c("3p", "4p"))]) %>%
  ggplot(aes(x = as.Date(date), y = y, color = income, linetype = model)) +
  scale_linetype(name = "Model") +
  geom_line(lwd = 1) +
  ylab("DIC") +
  xlab("Date") +
  theme_bw()

# calc_DIC <- function(out, model) {
#
#   # Combining Chains Together
#   chain_1 <- out$pmcmc_results$chains$chain1$results[5000:10000, ]
#   chain_2 <- out$pmcmc_results$chains$chain2$results[5000:10000, ]
#   chain_3 <- out$pmcmc_results$chains$chain3$results[5000:10000, ]
#   chain <- rbind(chain_1, chain_2, chain_3)
#
#   if (!(model %in% c("3_param", "4_param"))) {
#     stop("wrong model specification")
#   }
#
#   if (model == "3_param") {
#
#     # Calculating D_bar (posterior mean of the deviance)
#     D_bar <- mean(-2 * chain$log_posterior)
#
#     # Calculating D_hat (posterior deviance of the mean parameters)
#     mean_start_date <- round(mean(chain$start_date))
#     mean_R0 <- mean(chain$R0)
#     mean_Meff <- mean(chain$Meff)
#     mean_Meff_pl <- mean(chain$Meff_pl) # remove this once OJ's fixed
#
#     # Calculating Prior for Mean Parameters
#     pars <- c(mean_start_date, mean_R0, mean_Meff, mean_Meff_pl) # remove mean_Meff_pl once OJ's fixed
#     names(pars) <- c("start_date", "R0", "Meff", "Meff_pl")
#     prior <- out$pmcmc_results$inputs$prior(c(pars))
#
#     # Calculating Likelihood for Mean Parameters
#     data <- out$pmcmc_results$inputs$data
#     mean_pars <- list(start_date = squire:::offset_to_start_date(data$date[1], mean_start_date),
#                       R0 = mean_R0,
#                       Meff = mean_Meff,
#                       Meff_pl = mean_Meff_pl)
#     squire_model <- squire:::deterministic_model()
#     model_params <- out$pmcmc_results$inputs$model_params
#     par_obs <- out$pmcmc_results$inputs$pars_obs
#     n_particles <- out$pmcmc_results$inputs$n_particles
#     roll <- out$pmcmc_results$inputs$roll
#     scale_meff_pl <- out$pmcmc_results$inputs$scale_meff_pl
#     interventions <- out$pmcmc_results$inputs$interventions
#     loglik <- squire:::calc_loglikelihood(mean_pars, data, squire_model, model_params, par_obs, n_particles,
#                                           0, "ll", roll, scale_meff_pl, interventions)
#     loglik <- loglik$log_likelihood
#
#     # Calculating D_hat = deviance of mean parameters
#     D_hat <- -2 * (loglik + prior)
#
#     # Calculating the DIC
#     pD <- D_bar - D_hat
#     DIC <- D_bar + pD
#     return(DIC)
#
#   } else if (model == "4_param") {
#
#     # Calculating D_bar (posterior mean of the deviance)
#     D_bar <- mean(-2 * chain$log_posterior)
#
#     # Calculating D_hat (posterior deviance of the mean parameters)
#     mean_start_date <- round(mean(chain$start_date))
#     mean_R0 <- mean(chain$R0)
#     mean_Meff <- mean(chain$Meff)
#     mean_Meff_pl <- mean(chain$Meff_pl)
#
#     # Calculating Prior for Mean Parameters
#     pars <- c(mean_start_date, mean_R0, mean_Meff, mean_Meff_pl)
#     names(pars) <- c("start_date", "R0", "Meff", "Meff_pl")
#     prior <- out$pmcmc_results$inputs$prior(c(pars))
#
#     # Calculating Likelihood for Mean Parameters
#     data <- out$pmcmc_results$inputs$data
#     mean_pars <- list(start_date = squire:::offset_to_start_date(data$date[1], mean_start_date),
#                       R0 = mean_R0,
#                       Meff = mean_Meff,
#                       Meff_pl = mean_Meff_pl)
#     squire_model <- squire:::deterministic_model()
#     model_params <- out$pmcmc_results$inputs$model_params
#     par_obs <- out$pmcmc_results$inputs$pars_obs
#     n_particles <- out$pmcmc_results$inputs$n_particles
#     roll <- out$pmcmc_results$inputs$roll
#     scale_meff_pl <- out$pmcmc_results$inputs$scale_meff_pl
#     interventions <- out$pmcmc_results$inputs$interventions
#     loglik <- squire:::calc_loglikelihood(mean_pars, data, squire_model, model_params, par_obs, n_particles,
#                                           0, "ll", roll, scale_meff_pl, interventions)
#     loglik <- loglik$log_likelihood
#
#     # Calculating D_hat = deviance of mean parameters
#     D_hat <- -2 * (loglik + prior)
#
#     # Calculating DIC = D_bar + pD
#     pD <- D_bar - D_hat
#     DIC <- D_bar + pD
#     return(DIC)
#
#   }
# }
#
#
# dim(out$output)
# out$parameters
#
#
#
#
