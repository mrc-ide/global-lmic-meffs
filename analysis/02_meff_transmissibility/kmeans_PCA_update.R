## 01a Boosted Regression Tree
library(tidyverse); library(ggfortify); library(rgl); library(NbClust)
library(factoextra); library(patchwork); library(rnaturalearth); library(rnaturalearthdata)
devtools::load_all()

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

## 1. Get Parameters from the Fit 
date_0 <- "2020-07-04"
reports <- reports_4parameter_day(date_0)
parameters <- matrix(nrow = nrow(reports), ncol = 4)
for (i in 1:nrow(reports)) {
  four_out <- file.path(here::here(), "analysis/data/raw_data/server_results", "archive", "lmic_reports_google_pmcmc", reports$id[i], "grid_out.rds")
  four_out <- readRDS(four_out)
  chain_1 <- tail(four_out$pmcmc_results$chains$chain1$results, 5000)
  chain_2 <- tail(four_out$pmcmc_results$chains$chain2$results, 5000)
  chain_3 <- tail(four_out$pmcmc_results$chains$chain3$results, 5000)
  chain <- rbind(chain_1, chain_2, chain_3)
  R0 <- mean(chain$R0)
  start_date <- mean(chain$start_date)
  Meff <- mean(chain$Meff)
  Meff_pl <- mean(chain$Meff_pl)
  total_deaths <- sum(four_out$pmcmc_results$inputs$data$deaths)
  parameters[i, ] <- c(total_deaths, R0, Meff, Meff_pl)
  print(i)
}
colnames(parameters) <- c("total_deaths", "R0", "Meff", "Meff_pl")
row.names(parameters) <- reports$country

## 2. Get Mobility Data and Add in Minimum Mobility Reached
brt <- get_brt_predictions(date_0)
max_mob_red <- vapply(reports$country, function(x) {
  mobdf <- brt[[x]]
  m <- predict(loess(C~as.numeric(date), data=mobdf, span = 0.2), type = "response")
  mob_min <- min(m)
  return(mob_min)},
  numeric(1))
parameters <- cbind(parameters, max_mob_red)
colnames(parameters) <- c("total_deaths", "R0", "Meff", "Meff_pl", "max_mob_red")

## 3. Get ECDC Data and Add in Number of Deaths at Minimum Mobility 
ecdc <- get_ecdc(date_0) %>%
  mutate(continentExp = factor(continentExp), dateRep = as.Date(dateRep))
deaths_date_mob_min <- vapply(seq_along(reports$country), function(x) {
  iso <- reports$country[x]
  mobdf <- brt[[iso]]
  m <- predict(loess(C~as.numeric(date), data=mobdf, span = 0.2), type = "response")
  mob_min <- which(m == min(m))
  temp_date <- mobdf$date[mob_min]
  temp_ecdc <- ecdc[ecdc$countryterritoryCode == iso, ] %>%
    filter(dateRep <= temp_date)
  deaths <- sum(temp_ecdc$deaths, na.rm = TRUE)
  return(deaths)},
  numeric(1))
parameters <- cbind(parameters, deaths_date_mob_min)
colnames(parameters) <- c("total_deaths", "R0", "Meff", "Meff_pl", "max_mob_red", "deaths_date_mob_min")

deaths_per_case <- vapply(seq_along(reports$country), function(x) {
  iso <- reports$country[x]
  temp_ecdc <- ecdc[ecdc$countryterritoryCode == iso, ]
  cases <- sum(temp_ecdc$cases, na.rm = TRUE)
  deaths <- sum(temp_ecdc$deaths, na.rm = TRUE)
  return(cases/deaths)},
  numeric(1))
parameters <- cbind(parameters, deaths_per_case)
colnames(parameters) <- c("total_deaths", "R0", "Meff", "Meff_pl", "max_mob_red", "deaths_date_mob_min", "deaths_per_case")

parameters <- cbind(parameters, parameters[, "Meff"] - parameters[, "Meff_pl"])
colnames(parameters) <- c("total_deaths", "R0", "Meff", "Meff_pl", "max_mob_red", "deaths_date_mob_min", "deaths_per_case", "uncouple")

## 2. PCA and k-means to identify clusters
vars <- c("R0", "Meff", "Meff_pl", "max_mob_red", "deaths_date_mob_min", "deaths_per_case", "uncouple")
deaths_subset <- 50
subset_parameters <- parameters[parameters[, "total_deaths"] >= 50, ]
overall <- data.frame(iso = row.names(subset_parameters), 
                      country = countrycode::countrycode(row.names(subset_parameters), "iso3c", "country.name"), 
                      continent = countrycode::countrycode(row.names(subset_parameters), "iso3c", "continent"), 
                      subset_parameters[, vars])
pca_inputs <- overall[, vars]
pca_inputs$deaths_date_mob_min <- log(pca_inputs$deaths_date_mob_min + 1)
pca_inputs$deaths_per_case <- log(pca_inputs$deaths_per_case)
correlation_structure <- cor(pca_inputs)
pca_inputs <- apply(pca_inputs, 2, scale)
row.names(pca_inputs) <- overall$iso
pca <- prcomp(pca_inputs)
summary(pca)
contribution <- sweep(abs(pca$rotation), 2, colSums(abs(pca$rotation)), "/") * 100

# Selecting optimal number of clusters
NbClust(data = pca$x[, 1:3], diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

## simple kmeans on the reduced dimension with 6 clusters as a result
kmeans_out <- kmeans(pca$x[, 1:3], centers = 4)
a <- fviz_cluster(kmeans_out, data = pca$x[, 1:3], repel = TRUE, shape = "circle",
                  axes = c(1, 2)) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_blank())

# 
overall$cluster <- unname(kmeans_out$cluster)
summary <- overall %>%
  group_by(cluster) %>%
  summarise(R0 = median(R0),
            Meff = median(Meff),
            Meff_pl = median(Meff_pl),
            max_mob_red = median(max_mob_red),
            deaths_date_mob_min = log(median(deaths_date_mob_min) + 1),
            uncouple = median(uncouple),
            deaths_per_case = median(log(deaths_per_case)),
            n = n())
summary_plot <- summary %>%
  gather(metric, value, -cluster, -n)

summary_sd <- overall %>%
  group_by(cluster) %>%
  summarise(n = n(),
            R0_sd = sd(R0), 
            Meff_sd = sd(Meff)/sqrt(n), 
            Meff_pl_sd = sd(Meff_pl)/sqrt(n), 
            max_mob_red_sd = sd(max_mob_red)/sqrt(n),
            deaths_date_mob_min_sd = sd(log(deaths_date_mob_min + 1))/sqrt(n),
            uncouple_sd = sd(uncouple)/sqrt(n),
            deaths_per_case_sd = sd(log(deaths_per_case)/sqrt(n))) %>%
  gather(metric, sd, -cluster, -n)

summary_plot$sd <- summary_sd$sd 
summary_plot <- summary_plot %>%
  mutate(cluster = factor(cluster))
b <- ggplot(summary_plot, aes(x = cluster, y = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_wrap(~metric, scale = "free_y") + 
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0.2) +
  theme(legend.position = "none")

b
table(kmeans_out$cluster)

guide <- "AABBB
AABBB"
# 14 x 7
a + b +
  plot_layout(design = guide)


world <- ne_countries(scale = "medium", returnclass = "sf")
overall_red <- overall %>%
  select(iso, cluster)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols <- gg_color_hue(5)

new_world <- world %>%
  left_join(overall_red, by = c("iso_a3" = "iso")) %>%
  mutate(cluster = factor(cluster))
c <- ggplot(data = new_world) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_manual(values = c(cols), na.value = "light grey") +
  coord_sf(ylim = c(-55, 80)) +
  theme_bw() +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 


guide <- "##AAA
BBAAA
BBAAA
BBAAA
BBCCC
BBCCC
##CCC"
# 14 x 7
c + a + b +
  plot_layout(design = guide)

guide <- "BBAAA
BBAAA"

# 14 x 7
a + b +
  plot_layout(design = guide)



plot_overall <- overall %>%
  mutate(deaths_date_mob_min = log(deaths_date_mob_min + 1)) %>%
  gather(metric, value, -iso, -country, -continent, -cluster) %>%
  mutate(cluster = factor(cluster)) %>%
  filter(metric != "start_date")
ggplot(plot_overall, aes(x = cluster, y = value, col = cluster)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  facet_wrap(~metric, scale = "free_y") +
  geom_jitter(aes(x = cluster, y = value, fill = cluster), 
              pch = 21, size = 2, width = 0.2) +
  theme(legend.position = "none")


##
open3d() 
plot3d(pca$x[, 1:3], col = kmeans_out$cluster, size = 10, xlab = "", ylab = "", zlab = "", box = FALSE)
for (i in 1:4) {
  cov <- cov(pca$x[kmeans_out$cluster == i, 1:3])
  mean <- apply(pca$x[kmeans_out$cluster == i, 1:3], 2, mean)
  plot3d(ellipse3d(cov, centre = mean, level = 0.75, alpha = 0.5), col = palette()[i], alpha = 0.2, add = TRUE)
}

# silhoette method simplest
factoextra::fviz_nbclust(pca$x[, 1:3], kmeans, method = "silhouette")
gap_stat <- cluster::clusGap(pca$x[, 1:3], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
factoextra::fviz_gap_stat(gap_stat)
fviz_nbclust(pca$x[, 1:4], kmeans, method = c("silhouette", "wss", "gap_stat"))
fviz_nbclust(pca$x[, 1:4], kmeans, method = "wss")
fviz_nbclust(pca$x[, 1:4], kmeans, method = "silhouette")
fviz_nbclust(pca$x[, 1:4], kmeans, method = "gap_stat")



