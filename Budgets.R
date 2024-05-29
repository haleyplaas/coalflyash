setwd("Q:\\My Drive\\Code Repositories\\R\\coalflyash")
rm(list = ls())

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

#install.packages("")
# when I push to R I will need to remove the large data files
# Reading in the data ----------------------------------------------------------------------- 
sim.1 <- "Q:/My Drive/Collaborations/Coal Fly Ash/data/CAM6-MIMI-PI-RESICOAL0.2-FIRE33.cam.h1.2009-2011.nc"
sim.v1.PI <- nc_open(sim.1)
sim.2 <- "Q:/My Drive/Collaborations/Coal Fly Ash/data/CAM6-MIMI-PI-RESICOAL33-FIRE33.cam.h1.2009-2011.nc" # had to re-run because settings were accidentally wrong :( 
sim.v2.PI <- nc_open(sim.2)
sim.3 <-"Q:/My Drive/Collaborations/Coal Fly Ash/data/CAM6-MIMI-PI-RESICOAL33-FIRE56.cam.h1.2009-2011.nc" # this is actually RESICOAL0.2 per error src.cam soil_erod file -- check with douglas if more advantageous to re-run for direct comparison, or maybe when they submit re-runs we can align 
sim.v3.PI <- nc_open(sim.3)

# creating vectors of all possible longitude, latitude, and timepoint options -- long, lat, and lev are the same for all of Douglas' coal-fly-ash simulations, doubled checked by reading them in individually, but TIME is different and 1D -- 920 options for v1-3, but 1640 options for v4. # I think this means I will just need to calculate my medians offline because the values is already a composite value calculated online
all.long.options.PI <- ncvar_get(sim.v1.PI, "lon")
all.lat.options.PI <- ncvar_get(sim.v1.PI, "lat")
all.lev.options.PI <- ncvar_get(sim.v1.PI, "lev")
all.time.options.PI <- ncvar_get(sim.v1.PI, "time")

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- 180.0/length(all.lat.options.PI)
d.long <- 360/length(all.long.options.PI)
d.time <- 365/length(all.time.options.PI)

variables <- sim.v1.PI$var
variable_types <- sapply(variables, function(var) var$prec)
variable_info <- data.frame(
  Variable_Name = names(variable_types),
  Variable_Type = variable_types)

# Extract the necessary variables
# PI MIMI SIMULATION = PI.v1
variable.DD.PI.v1 <- ncvar_get(sim.v1.PI, "FESOLDRY")
variable.WD.PI.v1 <- ncvar_get(sim.v1.PI, "FESOLWET")

# PI SIMULATION v2
variable.DD.PI.v2 <- ncvar_get(sim.v2.PI, "FESOLDRY")
variable.WD.PI.v2 <- ncvar_get(sim.v2.PI, "FESOLWET")

# PI SIMULATION v3
variable.DD.PI.v3 <- ncvar_get(sim.v3.PI, "FESOLDRY")
variable.WD.PI.v3 <- ncvar_get(sim.v3.PI, "FESOLWET")

# Filter the data to consider only the first level when lev > 1 (surface altitude) 
# XYZ <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface
FESOLDRY.PI.v1 <- variable.DD.PI.v1[ , ,  ] 
FESOLWET.PI.v1 <- variable.WD.PI.v1[ , ,  ]

FESOLDRY.PI.v2 <- variable.DD.PI.v2[ , , ] 
FESOLWET.PI.v2 <- variable.WD.PI.v2[ , , ]

FESOLDRY.PI.v3 <- variable.DD.PI.v3[ , , ] 
FESOLWET.PI.v3 <- variable.WD.PI.v3[ , , ]

# Add Wet and Dry deposition together to get total deposition
FESOLDEP.PI.v1 <- FESOLDRY.PI.v1 + FESOLWET.PI.v1 
FESOLDEP.PI.v2 <- FESOLDRY.PI.v2 + FESOLWET.PI.v2 
FESOLDEP.PI.v3 <- FESOLDRY.PI.v3 + FESOLWET.PI.v3 

# Calculate the median over entire time period
#PI v1 (MIMI) 
median.SOLDEP.PI.v1 <- apply(FESOLDEP.PI.v1, c(1, 2), median, na.rm = TRUE)
#PI v2  
median.SOLDEP.PI.v2 <- apply(FESOLDEP.PI.v2, c(1, 2), median, na.rm = TRUE)
#PI v3 
median.SOLDEP.PI.v3 <- apply(FESOLDEP.PI.v3, c(1, 2), median, na.rm = TRUE)

# Closing netcdf files to save space for R processing
nc_close(sim.v1.PI)
nc_close(sim.v2.PI)
nc_close(sim.v3.PI)

# Finding budgets by ocean region  -------------------------------------------
# Create a dataframe with latitude, longitude, and annual MEDIAN
coords.med.PI <- data.frame(latitude = rep(all.lat.options.PI, each = length(all.long.options.PI)),
                         longitude = rep(all.long.options.PI, length(all.lat.options.PI)),
                         median_flux_v1 = as.vector(median.SOLDEP.PI.v1),
                         median_flux_v2 = as.vector(median.SOLDEP.PI.v2),
                         median_flux_v3 = as.vector(median.SOLDEP.PI.v3))

# if I need to pivot_wider to lat rows and lon columns df 
#coords_wide.med.PI.v1 <- pivot_wider(coords.med.PI.v1, names_from = longitude, values_from = median_flux)
#coords_wide.med.PI.v1 <- spread(coords.med.PI.v1, key = longitude, value = median_flux) 
#rownames(coords_wide.med.PI.v1) <- all.lat.options.PI 
#coords_wide.med.PI.v1 <- coords_wide.med.PI.v1[,-1]

# Step 1. finding area for each grid box 
r <- 6.371E6 #(radius of the earth in m)
coords.med.PI <- coords.med.PI %>%
  mutate(grid_area = (pi*r^2*(abs((sin(latitude)-sin((latitude+d.lat))))*abs((longitude-(longitude+d.long)))))/180)

# Step 2. calculating deposition flux for each individual grid box, and convert to kg/year
coords.med.PI <- coords.med.PI %>% mutate(budget_v1 = median_flux_v1*grid_area*60*60*24*365*1e-6,
                                          budget_v2 = median_flux_v2*grid_area*60*60*24*365*1e-6,
                                          budget_v3 = median_flux_v3*grid_area*60*60*24*365*1e-6)
# Step 3. Compile regionally
coords.med.PI$latitude <- as.numeric(coords.med.PI$latitude)
coords.med.PI$longitude <- as.numeric(coords.med.PI$longitude)

coords.med.PI <- coords.med.PI %>% 
  mutate(ocean_region = case_when(
    (longitude >= 295 & latitude < 0 & latitude > -45) | (longitude <= 30 & latitude < 0 & latitude > -45) ~ "SATL",  
    (longitude >= 265 & latitude > 25 & latitude < 55) | (longitude < 100 & latitude > 25 & latitude < 55) ~ "NATL", 
    longitude <= 100 & longitude > 78 & latitude < 30 & latitude > -15 ~ "Bay of Bengal",
    longitude <= 78 & longitude >= 30 & latitude < 30 & latitude > -15 ~ "Arabian Sea",
    longitude < 150 & longitude > 100 & latitude <= 60 & latitude > 0 ~ "SEAS", 
    longitude <= 265 & longitude > 150 & latitude < 55 & latitude >= 25 ~ "NPAC", 
    longitude >= 0 & latitude >= 55 & latitude <= 90 ~ "ARCT", 
    longitude < 295 & longitude > 135 & latitude <= 0 & latitude >= -45 ~ "AUSP", 
    longitude > 30 & longitude < 135 & latitude < -15 & latitude > -45 ~ "SIND", 
    longitude >= 0 & latitude <= -45 & latitude >= -90 ~ "SO",
    (longitude >= 150 & latitude > 0 & latitude < 25) | (longitude < 30 & latitude > 0 & latitude < 25) ~ "CPAO")) 

na_count <- sum(is.na(coords.med.PI$ocean_region))
print(na_count)
region_counts <- table(coords.med.PI$ocean_region)
print(region_counts)

annual.budgets.PI <- coords.med.PI %>% dplyr::select(budget_v1, budget_v2, budget_v3, ocean_region) %>% 
                                           group_by(ocean_region) %>% 
                                           summarise(across(everything(), sum, na.rm = T))




