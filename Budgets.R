setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

#install.packages("")
# Reading in the data ----------------------------------------------------------------------- 
## PREINDUSTRIAL SIMULATIONS
sim.1 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE33.cam.h1.2009-2011_PI.nc"
sim.v1.PI <- nc_open(sim.1)
sim.2 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33-FIRE33.cam.h1.2009-2011_PI.nc" # had to re-run because settings were accidentally wrong :( 
sim.v2.PI <- nc_open(sim.2)
sim.3 <-"C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE56.cam.h1.2009-2011_PI.nc" # this is actually RESICOAL0.2 per error src.cam soil_erod file -- check with douglas if more advantageous to re-run for direct comparison, or maybe when they submit re-runs we can align 
sim.v3.PI <- nc_open(sim.3)

## PRESENT DAY SIMULATIONS
sim.4 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h1.2009-2011_PD.nc"
sim.v1.PD <- nc_open(sim.4)
sim.5 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD10-OIL38.cam.h1.2009-2011_PD.nc"
sim.v2.PD <- nc_open(sim.5)
sim.6 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38.cam.h1.2009-2011_PD.nc"
sim.v3.PD <- nc_open(sim.6)
sim.7 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25.cam.h1.2009-2011_PD.nc"
sim.v4.PD <- nc_open(sim.7) # this one is missing
sim.8 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25-FIREFINE56.cam.h1.2009-2011_PD.nc"
sim.v5.PD <- nc_open(sim.8)

## extracting soluble iron and iron deposition fluxes from files ----------------------------------------------------------------------
# creating vectors of all possible longitude, latitude, and timepoint options -- long, lat, and lev are the same for all of Douglas' coal-fly-ash simulations, doubled checked by reading them in individually, but TIME is different and 1D -- 920 options for v1-3, but 1640 options for v4. # I think this means I will just need to calculate my medians offline because the values is already a composite value calculated online
all.long.options <- ncvar_get(sim.v1.PI, "lon")
all.lat.options <- ncvar_get(sim.v1.PI, "lat")
all.lev.options <- ncvar_get(sim.v1.PI, "lev")
all.time.options <- ncvar_get(sim.v1.PI, "time")

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- 180.0/length(all.lat.options)
d.long <- 360/length(all.long.options)
d.time <- 365/length(all.time.options)

# Extract the necessary variables
# PI MIMI SIMULATION = PI.v1
variable.SOL.DD.PI.v1 <- ncvar_get(sim.v1.PI, "FESOLDRY")
variable.SOL.WD.PI.v1 <- ncvar_get(sim.v1.PI, "FESOLWET")

# PI SIMULATION v2
variable.SOL.DD.PI.v2 <- ncvar_get(sim.v2.PI, "FESOLDRY")
variable.SOL.WD.PI.v2 <- ncvar_get(sim.v2.PI, "FESOLWET")

# PI SIMULATION v3
variable.SOL.DD.PI.v3 <- ncvar_get(sim.v3.PI, "FESOLDRY")
variable.SOL.WD.PI.v3 <- ncvar_get(sim.v3.PI, "FESOLWET")

# PD SIMULATION v1
variable.SOL.DD.PD.v1 <- ncvar_get(sim.v1.PD, "FESOLDRY")
variable.SOL.WD.PD.v1 <- ncvar_get(sim.v1.PD, "FESOLWET")

# PD SIMULATION v2
variable.SOL.DD.PD.v2 <- ncvar_get(sim.v2.PD, "FESOLDRY")
variable.SOL.WD.PD.v2 <- ncvar_get(sim.v2.PD, "FESOLWET")

# PD SIMULATION v3
variable.SOL.DD.PD.v3 <- ncvar_get(sim.v3.PD, "FESOLDRY")
variable.SOL.WD.PD.v3 <- ncvar_get(sim.v3.PD, "FESOLWET")

# PD SIMULATION v4
variable.SOL.DD.PD.v4 <- ncvar_get(sim.v4.PD, "FESOLDRY")
variable.SOL.WD.PD.v4 <- ncvar_get(sim.v4.PD, "FESOLWET")

# PD SIMULATION v5
variable.SOL.DD.PD.v5 <- ncvar_get(sim.v5.PD, "FESOLDRY")
variable.SOL.WD.PD.v5 <- ncvar_get(sim.v5.PD, "FESOLWET")

# Filter the data to consider only the first level when lev > 1 (surface altitude) ----------------------------------------------
# XYZ <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface
FESOLDRY.PI.v1 <- variable.SOL.DD.PI.v1[ , , ] 
FESOLWET.PI.v1 <- variable.SOL.WD.PI.v1[ , , ]

FESOLDRY.PI.v2 <- variable.SOL.DD.PI.v2[ , , ] 
FESOLWET.PI.v2 <- variable.SOL.WD.PI.v2[ , , ]

FESOLDRY.PI.v3 <- variable.SOL.DD.PI.v3[ , , ] 
FESOLWET.PI.v3 <- variable.SOL.WD.PI.v3[ , , ]

FESOLDRY.PD.v1 <- variable.SOL.DD.PD.v1[ , , ] 
FESOLWET.PD.v1 <- variable.SOL.WD.PD.v1[ , , ]

FESOLDRY.PD.v2 <- variable.SOL.DD.PD.v2[ , , ] 
FESOLWET.PD.v2 <- variable.SOL.WD.PD.v2[ , , ]

FESOLDRY.PD.v3 <- variable.SOL.DD.PD.v3[ , , ] 
FESOLWET.PD.v3 <- variable.SOL.WD.PD.v3[ , , ]

FESOLDRY.PD.v4 <- variable.SOL.DD.PD.v4[ , , ] 
FESOLWET.PD.v4 <- variable.SOL.WD.PD.v4[ , , ]

FESOLDRY.PD.v5 <- variable.SOL.DD.PD.v5[ , , ] 
FESOLWET.PD.v5 <- variable.SOL.WD.PD.v5[ , , ]

# Add Wet and Dry deposition together to get total deposition -------------------------------------------------------
FESOLDEP.PI.v1 <- FESOLDRY.PI.v1 + FESOLWET.PI.v1 

FESOLDEP.PI.v2 <- FESOLDRY.PI.v2 + FESOLWET.PI.v2 

FESOLDEP.PI.v3 <- FESOLDRY.PI.v3 + FESOLWET.PI.v3

FESOLDEP.PD.v1 <- FESOLDRY.PD.v1 + FESOLWET.PD.v1 

FESOLDEP.PD.v2 <- FESOLDRY.PD.v2 + FESOLWET.PD.v2 

FESOLDEP.PD.v3 <- FESOLDRY.PD.v3 + FESOLWET.PD.v3

FESOLDEP.PD.v4 <- FESOLDRY.PD.v4 + FESOLWET.PD.v4 

FESOLDEP.PD.v5 <- FESOLDRY.PD.v5 + FESOLWET.PD.v5 

# Calculate the median over entire time period -------------------------------------------------------------
#PI v1 (MIMI) 
median.SOLDEP.PI.v1 <- apply(FESOLDEP.PI.v1, c(1, 2), median, na.rm = TRUE)
#PI v2  
median.SOLDEP.PI.v2 <- apply(FESOLDEP.PI.v2, c(1, 2), median, na.rm = TRUE)
#PI v3 
median.SOLDEP.PI.v3 <- apply(FESOLDEP.PI.v3, c(1, 2), median, na.rm = TRUE)

#PD v1 (MIMI) 
median.SOLDEP.PD.v1 <- apply(FESOLDEP.PD.v1, c(1, 2), median, na.rm = TRUE)
#PD v2  
median.SOLDEP.PD.v2 <- apply(FESOLDEP.PD.v2, c(1, 2), median, na.rm = TRUE)
#PD v3 
median.SOLDEP.PD.v3 <- apply(FESOLDEP.PD.v3, c(1, 2), median, na.rm = TRUE)
#PD v4  
median.SOLDEP.PD.v4 <- apply(FESOLDEP.PD.v4, c(1, 2), median, na.rm = TRUE)
#PD v5 
median.SOLDEP.PD.v5 <- apply(FESOLDEP.PD.v5, c(1, 2), median, na.rm = TRUE)

# Closing netcdf files to save space for R processing
nc_close(sim.v1.PI)
nc_close(sim.v2.PI)
nc_close(sim.v3.PI)

nc_close(sim.v1.PD)
nc_close(sim.v2.PD)
nc_close(sim.v3.PD)
nc_close(sim.v4.PD)
nc_close(sim.v5.PD)

# Finding budgets by ocean region  -------------------------------------------
# Create a dataframe with latitude, longitude, and annual MEDIAN
coords.med.SOL <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                         longitude = rep(all.long.options, length(all.lat.options)),
                         median_flux_PI.v1 = as.vector(median.SOLDEP.PI.v1),
                         median_flux_PI.v2 = as.vector(median.SOLDEP.PI.v2),
                         median_flux_PI.v3 = as.vector(median.SOLDEP.PI.v3),
                         median_flux_PD.v1 = as.vector(median.SOLDEP.PD.v1),
                         median_flux_PD.v2 = as.vector(median.SOLDEP.PD.v2),
                         median_flux_PD.v3 = as.vector(median.SOLDEP.PD.v3),
                         median_flux_PD.v4 = as.vector(median.SOLDEP.PD.v4),
                         median_flux_PD.v5 = as.vector(median.SOLDEP.PD.v5))

# if I need to pivot_wider to lat rows and lon columns df 
#coords_wide.med.PI.v1 <- pivot_wider(coords.med.PI.v1, names_from = longitude, values_from = median_flux)
#coords_wide.med.PI.v1 <- spread(coords.med.PI.v1, key = longitude, value = median_flux) 
#rownames(coords_wide.med.PI.v1) <- all.lat.options.PI 
#coords_wide.med.PI.v1 <- coords_wide.med.PI.v1[,-1]

# Step 1. finding area for each grid box 
r <- 6.371E6 #(radius of the earth in m)
coords.med.SOL <- coords.med.SOL %>%
  mutate(grid_area = (pi*r^2*(abs((sin(latitude)-sin((latitude+d.lat))))*abs((longitude-(longitude+d.long)))))/180)

# Step 2. calculating deposition flux for each individual grid box, and convert to kg/year
coords.med.SOL <- coords.med.SOL %>% mutate(budget_PI.v1 = median_flux_PI.v1*grid_area*60*60*24*365*1e-6,
                                          budget_PI.v2 = median_flux_PI.v2*grid_area*60*60*24*365*1e-6,
                                          budget_PI.v3 = median_flux_PI.v3*grid_area*60*60*24*365*1e-6,
                                          budget_PD.v1 = median_flux_PD.v1*grid_area*60*60*24*365*1e-6,
                                          budget_PD.v2 = median_flux_PD.v2*grid_area*60*60*24*365*1e-6,
                                          budget_PD.v3 = median_flux_PD.v3*grid_area*60*60*24*365*1e-6,
                                          budget_PD.v4 = median_flux_PD.v4*grid_area*60*60*24*365*1e-6,
                                          budget_PD.v5 = median_flux_PD.v5*grid_area*60*60*24*365*1e-6)

# Step 3. Compile regionally
coords.med.SOL$latitude <- as.numeric(coords.med.SOL$latitude)
coords.med.SOL$longitude <- as.numeric(coords.med.SOL$longitude)

coords.med.SOL <- coords.med.SOL %>% 
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

na_count <- sum(is.na(coords.med.SOL$ocean_region))
print(na_count)
region_counts <- table(coords.med.SOL$ocean_region)
print(region_counts)

annual.SOL.budgets <- coords.med.SOL %>% 
                        dplyr::select(budget_PI.v1, budget_PI.v2, budget_PI.v3, budget_PD.v1, budget_PD.v2, budget_PD.v3, budget_PD.v4, budget_PD.v5, ocean_region) %>% group_by(ocean_region) %>% summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))


