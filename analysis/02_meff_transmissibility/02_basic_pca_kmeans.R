## 01a Boosted Regression Tree

library(globallmicmeffs)
library(tidyverse)

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

## 1. Get the correct meff data, ecdc data, google mobility data

reports <- reports_4parameter_day(date_0)
meffs <- pbapply::pblapply(seq_along(reports$id), function(x) {

  iso <- reports$country[x]
  out <- file.path(here::here(),
                   "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc",
                   reports$id[x], "grid_out.rds")
  out <- readRDS(out)
  rt <- extract_Rt(out, iso)

  Rt_tail <- (group_by(rt, rep) %>% summarise(Rt = tail(Rt,1)))$Rt
  Rt_min <- (group_by(rt, rep) %>% summarise(Rt = min(Rt)))$Rt

  df <- data.frame(
    "R0" = out$replicate_parameters$R0,
    "Rt" = Rt_tail,
    "Rt_min" = Rt_min,
    "Meff" = out$replicate_parameters$Meff,
    "date" = as.character(out$replicate_parameters$start_date),
    "iso" = iso,
    "max_mobility_change" = max(out$interventions$R0_change)-min(out$interventions$R0_change),
    stringsAsFactors = FALSE)

  return(df)
})


# format data
meff_all <- do.call(rbind, meffs)
meff_all$continent <- countrycode::countrycode(meff_all$iso, "iso3c", "continent")
meff_all$country <- countrycode::countrycode(meff_all$iso, "iso3c", "country.name")

# turn date into numeric for days since start of year
meff_all$date <- as.numeric(as.Date(meff_all$date)) - as.numeric(as.Date("2020-01-01"))

# group and summarise so have one value per country
meff_sum <- group_by(meff_all, iso, continent, country) %>%
  summarise_all(mean) %>%
  ungroup %>%
  as.data.frame()
rownames(meff_sum) <- meff_sum$country

# leave out hospital capacities at the moment
# meff_sum$hosp <- vapply(meff_sum$iso, function(x) {
#   squire:::get_hosp_bed_capacity(squire::get_population(iso3c = x)$country[1])
# }, numeric(1))
# meff_sum$icu <- vapply(meff_sum$iso, function(x) {
#   squire:::get_ICU_bed_capacity(squire::get_population(iso3c = x)$country[1])
# }, numeric(1))
# meff_sum$hosp <- meff_sum$hosp/sum(meff_sum$hosp)
# meff_sum$icu <- meff_sum$icu/sum(meff_sum$icu)

# death_dates
ecdc <- get_ecdc(date_0)
meff_sum$death_date <- vapply(meff_sum$iso, function(x) {
  e <- ecdc[ecdc$countryterritoryCode==x, ]
  as.Date(e$dateRep[max(which(e$deaths>0))])
}, numeric(1)) - as.numeric(as.Date("2020-01-01"))

## 2. PCA and k-means to identify clusters

## simple pca
pca <- prcomp(meff_sum[,(4:ncol(meff_sum))],scale. = TRUE)
#pca <- prcomp(meff_sum[,c(4,7,8)],scale. = TRUE)
library(ggfortify)
pca_plot <- autoplot(pca, data = meff_sum, colour = 'continent', label = TRUE,
         label.repel = FALSE, label.show.legend = FALSE) + theme_bw()

# what is the contribution of our covariates to the components
contribution <- sweep(abs(pca$rotation), 2, colSums(abs(pca$rotation)), "/") * 100

# Need to look at how many clusters though
library(factoextra)

## I tend to use both the simple silhouette method and the gap statistic approach
## and look for agreement. In general if I see a second obvious peak at high clusters
## I go for that

# silhoette method simplest
factoextra::fviz_nbclust(pca_plot$data[,1:2], kmeans, method = "silhouette")

# gap statistic method
gap_stat <- cluster::clusGap(pca_plot$data[,1:2], FUN = kmeans, nstart = 25,
                             K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## simple kmeans on the reduced dimension with 7 clusters as a result
kmeans_out <- kmeans(pca_plot$data[,1:2], centers = 7)
clust <- fviz_cluster(kmeans_out, data = pca_plot$data[,1:2], repel = TRUE) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_blank())

## 3. Work out the centroids parmeters as the mean of the values of countries in clusters

centers <- kmeans_out$centers
parms <- t(t(centers %*% t(pca$rotation[,1:2])) * pca$scale + pca$center)



##  ------------------------------
## Plotting ------------------------------
##  ------------------------------

# check size before saving
x11(width = 12,height = 12)
clust
cowplot::save_plot(plot = clust,
                   filename = file.path(here::here(),"analysis/figures/03d_clustering.png"),
                   base_height = 12,
                   base_width = 12)

cowplot::save_plot(plot = clust,
                   filename = file.path(here::here(),"analysis/figures/03d_clustering.svg"),
                   base_height = 12,
                   base_width = 12)
