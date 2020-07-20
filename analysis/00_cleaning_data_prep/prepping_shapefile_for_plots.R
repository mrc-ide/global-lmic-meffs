## 01c Plotting Maps of Start Dates and R0

# library(globallmicmeffs)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(viridis)
library(plyr)
library(fields)
library(squire)
library(rmapshaper)
library(rgeos)
library(broom)
library(data.table)
library(sf)

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

# get the reports
date_0 <- "2020-07-04"

grids <- out_3parameter_list(date_0)

param_sums <- lapply(seq_along(grids), function(i) {

  x <- grids[[i]]
  pars <- x$replicate_parameters
  pars$start_date <- as.numeric(pars$start_date)

  q_025 <- function(x) { quantile(x, 0.025)}
  q_975 <- function(x) { quantile(x, 0.975)}

  y <- summarise_all(pars, "mean")
  ymin <- summarise_all(pars, q_025)
  ymax <- summarise_all(pars, q_975)
  df <- data.frame("var" = names(y),
                   "y" = as.numeric(y),
                   "ymin" = as.numeric(ymin),
                   "ymax" = as.numeric(ymax))
  df$iso <- names(grids)[i]
  return(df)
})

params <- do.call(rbind, param_sums)


##  ------------------------------
## Plotting ------------------------------
##  ------------------------------

## Plot to show the global range in R0 and start date
global_shp_all <- readOGR("analysis/data/shp/gadm36_0.shp", stringsAsFactors = FALSE)
#Add in manually the data we want
iso_we_can_map <- global_shp_all$GID_0
iso_we_can_map <- iso_we_can_map[which(iso_we_can_map %in% params$iso)]

LMIC_shp_data_add <- do.call(rbind, sapply(iso_we_can_map, function(x){
  print(x)

  data_here <- params[which(params$iso == x), ]
  data_here2 <- data.frame(t(as.data.frame(unlist(data_here[, 2:4]))))
  colnames(data_here2) <- paste0(as.character(data_here[, 1]), rep(c("_mid", "_min", "_max"),
                                                                   each = 3))
  data_here2

}, simplify = FALSE))

#Remove places without data from the shapefile
blank_data <- matrix(rep(NA, ncol(LMIC_shp_data_add) * nrow(global_shp_all)), ncol = ncol(LMIC_shp_data_add))
colnames(blank_data) <- names(LMIC_shp_data_add)

for(i in 1:nrow(LMIC_shp_data_add)){
  blank_data[which(global_shp_all$GID_0 == iso_we_can_map[i]), ] <- as.numeric(LMIC_shp_data_add[i, ])
}

global_shp_all_data <- SpatialPolygonsDataFrame(global_shp_all,
                                                data = cbind(as.data.frame(global_shp_all), blank_data))

#Now we simplify otherwise it'll take weeks
#Remove tiny countries
all_countries_size <- do.call(rbind, sapply(1:nrow(global_shp_all_data), function(y){
  print(y)
  data.frame(ISO = global_shp_all_data$GID_0[y], area_km2 =  area(global_shp_all_data[y, ])/1024^2,
             stringsAsFactors = FALSE)
}, simplify = FALSE))

#Smallest place we have data for
LMIC_sizes <- all_countries_size[which(all_countries_size$ISO %in% row.names(LMIC_shp_data_add)), ]
LMIC_sizes$ISO[which.min(LMIC_sizes$area_km2)]
LMIC_sizes[order(LMIC_sizes$area_km2), ]

#Maldives is tiny and wont show up on a map, so let's go for the 2nd smallest as the cutoff
#STP

#What countries are smaller
smallest_countries <- all_countries_size[which(all_countries_size$area_km2 < min(LMIC_sizes$area_km2[LMIC_sizes$area_km2!=min(LMIC_sizes$area_km2)])), ]

#Ditch them and antarctica
global_adjusted <- global_shp_all_data[-which(global_shp_all_data$GID_0 %in% c("ATA", smallest_countries$ISO)), ]
plot(global_adjusted)

#Can probably get away with removing anything smaller than the falklands
smaller_than_falklands <- all_countries_size[which(all_countries_size$area_km2 <=
                                                     all_countries_size[which(all_countries_size$ISO == "FLK"), ]$area_km2), ]

global_adjusted_v2 <- global_adjusted[-which(global_adjusted$GID_0 %in% smaller_than_falklands$ISO), ]
global_sizeable_data <- gSimplify(global_adjusted_v2, tol = 0.01, topologyPreserve = T)

#Now made into a spatial polygon data frame
global_shp_all_data_simp <- SpatialPolygonsDataFrame(global_sizeable_data, data = as.data.frame(global_adjusted_v2))
#This fills in the holes
global_shp_all_data_simp_buff <- gBuffer(global_shp_all_data_simp, byid = TRUE, width = 0)

#Save shapfile for later
writeOGR(obj = global_shp_all_data_simp_buff,
         ".",
         "analysis/data/shp/global_shapefile_small_islands_removed.shp",
         driver = "ESRI Shapefile")

#To make it back into a dataframe
global_shp_simp_df <- tidy(global_shp_all_data_simp_buff, region = "GID_0") %>% data.table()
colnames(global_shp_simp_df)[which(colnames(global_shp_simp_df) == "id")] <- "GID_0"

global_shp_simp_df_data <- join(global_shp_simp_df, as.data.frame(global_shp_all_data_simp), by = "GID_0")

write.csv(global_shp_simp_df_data, "analysis/data/map_plot_data/start_R0_Meff_date.csv", row.names = FALSE)






