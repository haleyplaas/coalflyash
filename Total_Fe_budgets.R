setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())
.rs.restartR() # turn on when need to clear up RAM space

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
variable.TOT.DD.PI.v1 <- ncvar_get(sim.v1.PI, "FETOTDRY")
variable.TOT.WD.PI.v1 <- ncvar_get(sim.v1.PI, "FETOTWET")

# PI SIMULATION v2
variable.TOT.DD.PI.v2 <- ncvar_get(sim.v2.PI, "FETOTDRY")
variable.TOT.WD.PI.v2 <- ncvar_get(sim.v2.PI, "FETOTWET")

# PI SIMULATION v3
variable.TOT.DD.PI.v3 <- ncvar_get(sim.v3.PI, "FETOTDRY")
variable.TOT.WD.PI.v3 <- ncvar_get(sim.v3.PI, "FETOTWET")

# PD SIMULATION v1
variable.TOT.DD.PD.v1 <- ncvar_get(sim.v1.PD, "FETOTDRY")
variable.TOT.WD.PD.v1 <- ncvar_get(sim.v1.PD, "FETOTWET")

# PD SIMULATION v2
variable.TOT.DD.PD.v2 <- ncvar_get(sim.v2.PD, "FETOTDRY")
variable.TOT.WD.PD.v2 <- ncvar_get(sim.v2.PD, "FETOTWET")

# PD SIMULATION v3
variable.TOT.DD.PD.v3 <- ncvar_get(sim.v3.PD, "FETOTDRY")
variable.TOT.WD.PD.v3 <- ncvar_get(sim.v3.PD, "FETOTWET")

# PD SIMULATION v4
variable.TOT.DD.PD.v4 <- ncvar_get(sim.v4.PD, "FETOTDRY")
variable.TOT.WD.PD.v4 <- ncvar_get(sim.v4.PD, "FETOTWET")

# PD SIMULATION v5
variable.TOT.DD.PD.v5 <- ncvar_get(sim.v5.PD, "FETOTDRY")
variable.TOT.WD.PD.v5 <- ncvar_get(sim.v5.PD, "FETOTWET")

# Filter the data to consider only the first level when lev > 1 (surface altitude) ----------------------------------------------
# XYZ <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface
FETOTDRY.PI.v1 <- variable.TOT.DD.PI.v1[ , , ] 
FETOTWET.PI.v1 <- variable.TOT.WD.PI.v1[ , , ]

FETOTDRY.PI.v2 <- variable.TOT.DD.PI.v2[ , , ] 
FETOTWET.PI.v2 <- variable.TOT.WD.PI.v2[ , , ]

FETOTDRY.PI.v3 <- variable.TOT.DD.PI.v3[ , , ] 
FETOTWET.PI.v3 <- variable.TOT.WD.PI.v3[ , , ]

FETOTDRY.PD.v1 <- variable.TOT.DD.PD.v1[ , , ] 
FETOTWET.PD.v1 <- variable.TOT.WD.PD.v1[ , , ]

FETOTDRY.PD.v2 <- variable.TOT.DD.PD.v2[ , , ] 
FETOTWET.PD.v2 <- variable.TOT.WD.PD.v2[ , , ]

FETOTDRY.PD.v3 <- variable.TOT.DD.PD.v3[ , , ] 
FETOTWET.PD.v3 <- variable.TOT.WD.PD.v3[ , , ]

FETOTDRY.PD.v4 <- variable.TOT.DD.PD.v4[ , , ] 
FETOTWET.PD.v4 <- variable.TOT.WD.PD.v4[ , , ]

FETOTDRY.PD.v5 <- variable.TOT.DD.PD.v5[ , , ] 
FETOTWET.PD.v5 <- variable.TOT.WD.PD.v5[ , , ]

# Add Wet and Dry deposition together to get total deposition -------------------------------------------------------
FETOTDEP.PI.v1 <- FETOTDRY.PI.v1 + FETOTWET.PI.v1

FETOTDEP.PI.v2 <- FETOTDRY.PI.v2 + FETOTWET.PI.v2

FETOTDEP.PI.v3 <- FETOTDRY.PI.v3 + FETOTWET.PI.v3

FETOTDEP.PD.v1 <- FETOTDRY.PD.v1 + FETOTWET.PD.v1

FETOTDEP.PD.v2 <- FETOTDRY.PD.v2 + FETOTWET.PD.v2

FETOTDEP.PD.v3 <- FETOTDRY.PD.v3 + FETOTWET.PD.v3

FETOTDEP.PD.v4 <- FETOTDRY.PD.v4 + FETOTWET.PD.v4

FETOTDEP.PD.v5 <- FETOTDRY.PD.v5 + FETOTWET.PD.v5

# Calculate the median over entire time period -------------------------------------------------------------
#PI v1 (MIMI) 
median.TOTDEP.PI.v1 <- apply(FETOTDEP.PI.v1, c(1, 2), median, na.rm = TRUE)
#PI v2  
median.TOTDEP.PI.v2 <- apply(FETOTDEP.PI.v2, c(1, 2), median, na.rm = TRUE)
#PI v3 
median.TOTDEP.PI.v3 <- apply(FETOTDEP.PI.v3, c(1, 2), median, na.rm = TRUE)

#PD v1 (MIMI) 
median.TOTDEP.PD.v1 <- apply(FETOTDEP.PD.v1, c(1, 2), median, na.rm = TRUE)
#PD v2  
median.TOTDEP.PD.v2 <- apply(FETOTDEP.PD.v2, c(1, 2), median, na.rm = TRUE)
#PD v3 
median.TOTDEP.PD.v3 <- apply(FETOTDEP.PD.v3, c(1, 2), median, na.rm = TRUE)
#PD v4  
median.TOTDEP.PD.v4 <- apply(FETOTDEP.PD.v4, c(1, 2), median, na.rm = TRUE)
#PD v5 
median.TOTDEP.PD.v5 <- apply(FETOTDEP.PD.v5, c(1, 2), median, na.rm = TRUE)

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
coords.med.TOT <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
longitude = rep(all.long.options, length(all.lat.options)),
median_flux_PI.v1 = as.vector(median.TOTDEP.PI.v1),
median_flux_PI.v2 = as.vector(median.TOTDEP.PI.v2),
median_flux_PI.v3 = as.vector(median.TOTDEP.PI.v3),
median_flux_PD.v1 = as.vector(median.TOTDEP.PD.v1),
median_flux_PD.v2 = as.vector(median.TOTDEP.PD.v2),
median_flux_PD.v3 = as.vector(median.TOTDEP.PD.v3),
median_flux_PD.v4 = as.vector(median.TOTDEP.PD.v4),
median_flux_PD.v5 = as.vector(median.TOTDEP.PD.v5))

# if I need to pivot_wider to lat rows and lon columns df 
#coords_wide.med.PI.v1 <- pivot_wider(coords.med.PI.v1, names_from = longitude, values_from = median_flux)
#coords_wide.med.PI.v1 <- spread(coords.med.PI.v1, key = longitude, value = median_flux) 
#rownames(coords_wide.med.PI.v1) <- all.lat.options.PI 
#coords_wide.med.PI.v1 <- coords_wide.med.PI.v1[,-1]

# Step 1. finding area for each grid box 
r <- 6.371E6 #(radius of the earth in m)
coords.med.TOT <- coords.med.TOT %>%
mutate(grid_area = (pi*r^2*(abs((sin(latitude)-sin((latitude+d.lat))))*abs((longitude-(longitude+d.long)))))/180)

# Step 2. calculating deposition flux for each individual grid box, and convert to kg/year
coords.med.TOT <- coords.med.TOT %>% mutate(budget_PI.v1 = median_flux_PI.v1*grid_area*60*60*24*365*1e-6,
                                            budget_PI.v2 = median_flux_PI.v2*grid_area*60*60*24*365*1e-6,
                                            budget_PI.v3 = median_flux_PI.v3*grid_area*60*60*24*365*1e-6,
                                            budget_PD.v1 = median_flux_PD.v1*grid_area*60*60*24*365*1e-6,
                                            budget_PD.v2 = median_flux_PD.v2*grid_area*60*60*24*365*1e-6,
                                            budget_PD.v3 = median_flux_PD.v3*grid_area*60*60*24*365*1e-6,
                                            budget_PD.v4 = median_flux_PD.v4*grid_area*60*60*24*365*1e-6,
                                            budget_PD.v5 = median_flux_PD.v5*grid_area*60*60*24*365*1e-6)

# Step 3. Compile regionally
coords.med.TOT$latitude <- as.numeric(coords.med.TOT$latitude)
coords.med.TOT$longitude <- as.numeric(coords.med.TOT$longitude)

coords.med.TOT <- coords.med.TOT %>% 
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

na_count <- sum(is.na(coords.med.TOT$ocean_region))
print(na_count)
region_counts <- table(coords.med.TOT$ocean_region)
print(region_counts)

annual.TOT.budgets <- coords.med.TOT %>% 
dplyr::select(budget_PI.v1, budget_PI.v2, budget_PI.v3, budget_PD.v1, budget_PD.v2, budget_PD.v3, budget_PD.v4, budget_PD.v5, ocean_region) %>% group_by(ocean_region) %>% summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))


