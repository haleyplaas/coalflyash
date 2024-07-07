setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())
#.rs.restartR() # turn on when need to clear up RAM space

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

#install.packages("")
# Reading in the data ----------------------------------------------------------------------- 
## PRESENT DAY SIMULATIONS
sim.1 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38_2009-2011_DEPMEAN.nc"
sim.v1 <- nc_open(sim.1)
sim.2 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38_2009-2011_DEPMEAN.nc"
sim.v2 <- nc_open(sim.2)
sim.3 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD10-OIL38_2009-2011_DEPMEAN.nc"
sim.v3 <- nc_open(sim.3)
sim.4 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25_2009-2011_DEPMEAN.nc"
sim.v4 <- nc_open(sim.4) # this one is missing
#sim.5 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25-FIREFINE56.cam.h1.2009-2011_PD.nc"
#sim.v5 <- nc_open(sim.5)
## PREINDUSTRIAL SIMULATIONS
sim.6 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2_FINEFIRE33_v2.cam.h1.2009-2011_fire_x2.nc"
sim.v6 <- nc_open(sim.6)
sim.7 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE33_v2.cam.h1.2009-2011_fire_x2.nc"
sim.v7 <- nc_open(sim.7)
sim.8 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE56_v2.cam.h1.2009-2011_fire_x2.nc"
sim.v8 <- nc_open(sim.8)

## extracting soluble iron and iron deposition fluxes from files ----------------------------------------------------------------------
# creating vectors of all possible longitude, latitude, and timepoint options -- long, lat, and lev are the same for all of Douglas' coal-fly-ash simulations, doubled checked by reading them in individually, but TIME is different and 1D -- 920 options for v1-3, but 1640 options for v4. # I think this means I will just need to calculate my medians offline because the values is already a composite value calculated online
all.long.options <- ncvar_get(sim.v1, "lon")
all.lat.options <- ncvar_get(sim.v1, "lat")
all.lev.options <- ncvar_get(sim.v1, "lev")
all.time.options <- ncvar_get(sim.v1, "time")

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- 180.0/length(all.lat.options)
d.long <- 360/length(all.long.options)
d.time <- length(all.time.options)/365

# Extract the necessary variables
# PD SIMULATION v1
variable.SOL.DD.v1 <- ncvar_get(sim.v1, "FESOLDRY")
variable.SOL.WD.v1 <- ncvar_get(sim.v1, "FESOLWET")

# PD SIMULATION v2
variable.SOL.DD.v2 <- ncvar_get(sim.v2, "FESOLDRY")
variable.SOL.WD.v2 <- ncvar_get(sim.v2, "FESOLWET")

# PD SIMULATION v3
variable.SOL.DD.v3 <- ncvar_get(sim.v3, "FESOLDRY")
variable.SOL.WD.v3 <- ncvar_get(sim.v3, "FESOLWET")

# PD SIMULATION v4
variable.SOL.DD.v4 <- ncvar_get(sim.v4, "FESOLDRY")
variable.SOL.WD.v4 <- ncvar_get(sim.v4, "FESOLWET")

# PD SIMULATION v5
#variable.SOL.DD.v5 <- ncvar_get(sim.v5, "FESOLDRY")
#variable.SOL.WD.v5 <- ncvar_get(sim.v5, "FESOLWET")

# PI MIMI SIMULATION 
variable.SOL.DD.v6 <- ncvar_get(sim.v6, "FESOLDRY")
variable.SOL.WD.v6 <- ncvar_get(sim.v6, "FESOLWET")

# PI SIMULATION v2
variable.SOL.DD.v7 <- ncvar_get(sim.v7, "FESOLDRY")
variable.SOL.WD.v7 <- ncvar_get(sim.v7, "FESOLWET")

# PI SIMULATION v3
variable.SOL.DD.v8 <- ncvar_get(sim.v8, "FESOLDRY")
variable.SOL.WD.v8 <- ncvar_get(sim.v8, "FESOLWET")

# Filter the data to consider only the first level when lev > 1 (surface altitude) ----------------------------------------------
# XYZ <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface if needed
# if you plug in specific index values [1:288, 1:192, 1:1095] this works, blank just means selecting all 
FESOLDRY.v1 <- variable.SOL.DD.v1[ ,  ] 
FESOLWET.v1 <- variable.SOL.WD.v1[ ,  ]

FESOLDRY.v2 <- variable.SOL.DD.v2[ ,  ] 
FESOLWET.v2 <- variable.SOL.WD.v2[ ,  ]

FESOLDRY.v3 <- variable.SOL.DD.v3[ ,  ] 
FESOLWET.v3 <- variable.SOL.WD.v3[ ,  ]

FESOLDRY.v4 <- variable.SOL.DD.v4[ ,  ] 
FESOLWET.v4 <- variable.SOL.WD.v4[ ,  ]

#FESOLDRY.v5 <- variable.SOL.DD.v5[ , , ] 
#FESOLWET.v5 <- variable.SOL.WD.v5[ , , ]

FESOLDRY.v6 <- variable.SOL.DD.v6[ , , ] 
FESOLWET.v6 <- variable.SOL.WD.v6[ , , ]

FESOLDRY.v7 <- variable.SOL.DD.v7[ , , ] 
FESOLWET.v7 <- variable.SOL.WD.v7[ , , ]

FESOLDRY.v8 <- variable.SOL.DD.v8[ , , ] 
FESOLWET.v8 <- variable.SOL.WD.v8[ , , ]


# Add Wet and Dry deposition together to get total deposition -------------------------------------------------------
FESOLDEP.v1 <- FESOLDRY.v1 + FESOLWET.v1 

FESOLDEP.v2 <- FESOLDRY.v2 + FESOLWET.v2 

FESOLDEP.v3 <- FESOLDRY.v3 + FESOLWET.v3

FESOLDEP.v4 <- FESOLDRY.v4 + FESOLWET.v4 

#FESOLDEP.v5 <- FESOLDRY.v5 + FESOLWET.v5 

FESOLDEP.v6 <- FESOLDRY.v6 + FESOLWET.v6

FESOLDEP.v7 <- FESOLDRY.v7 + FESOLWET.v7 

FESOLDEP.v8 <- FESOLDRY.v8 + FESOLWET.v8 

# Calculate the mean over entire time period -------------------------------------------------------------
#PI v1 (MIMI) 
#                                            c(1, 2) indicates calculating over the first and second dimension (lon and lat)
#change this to mean 
# try writing for loop to calculate values (in kg per day) for each day and then find means 
#PD v1 (MIMI) 
mean.SOLDEP.v1 <- apply(FESOLDEP.v1, c(1, 2), mean, na.rm = TRUE) 
#PD v2  
mean.SOLDEP.v2 <- apply(FESOLDEP.v2, c(1, 2), mean, na.rm = TRUE)
#PD v3 
mean.SOLDEP.v3 <- apply(FESOLDEP.v3, c(1, 2), mean, na.rm = TRUE)
#PD v4  
mean.SOLDEP.v4 <- apply(FESOLDEP.v4, c(1, 2), mean, na.rm = TRUE)
#PD v5 
#mean.SOLDEP.v5 <- apply(FESOLDEP.v5, c(1, 2), mean, na.rm = TRUE)
# PI v1
mean.SOLDEP.v6 <- apply(FESOLDEP.v6, c(1, 2), mean, na.rm = TRUE)
#PI v2  
mean.SOLDEP.v7 <- apply(FESOLDEP.v7, c(1, 2), mean, na.rm = TRUE)
#PI v3 
mean.SOLDEP.v8 <- apply(FESOLDEP.v8, c(1, 2), mean, na.rm = TRUE)

# Closing netcdf files to save space for R processing
nc_close(sim.v1)
nc_close(sim.v2)
nc_close(sim.v3)
nc_close(sim.v4)
#nc_close(sim.v5)
nc_close(sim.v6)
nc_close(sim.v7)
nc_close(sim.v8)

# Finding budgets by ocean region  -------------------------------------------
# Create a dataframe with latitude, longitude, and annual MEDIAN
coords.med.SOL <- data.frame(Latitude = rep(all.lat.options, each = length(all.long.options)),
                         Longitude = rep(all.long.options, length(all.lat.options)),
                         mean_flux.v1 = as.vector(mean.SOLDEP.v1),
                         mean_flux.v2 = as.vector(mean.SOLDEP.v2),
                         mean_flux.v3 = as.vector(mean.SOLDEP.v3),
                         mean_flux.v4 = as.vector(mean.SOLDEP.v4),
                        # mean_flux.v5 = as.vector(mean.SOLDEP.v5),
                         mean_flux.v6 = as.vector(mean.SOLDEP.v6),
                         mean_flux.v7 = as.vector(mean.SOLDEP.v7),
                         mean_flux.v8 = as.vector(mean.SOLDEP.v8))

# Step 1. finding area for each grid box 
grid.areas <- expand.grid(all.lat.options, all.long.options)
names(grid.areas) <- c("Latitude", "Longitude")
grid.areas <- grid.areas %>% mutate(Latitude.rads = Latitude*(pi/180), Longitude.rads = Longitude*(pi/180))
r <- 6.37816E6 #(radius of the earth in m)
d.lat.rads <- d.lat*(pi/180) # convert to radians
d.long.rads <- d.long*(pi/180) # convert to radians

grid.areas <- grid.areas %>% #filter(Latitude.rads <= 0) %>%
  mutate(area = (r^2) * (sin(Latitude.rads + d.lat.rads) - sin(Latitude.rads)) * ((Longitude.rads + d.long.rads) - (Longitude.rads))) %>% 
  mutate(area = ifelse(area < 0, abs(area), area)) # last value (Latitude 90), was negative, so had to take abs

# add everything together and then check with actual total surface area of the earth 
coords.med.SOL <- coords.med.SOL %>% mutate(Latitude.rads = Latitude*(pi/180), Longitude.rads = Longitude*(pi/180))

coords.med.SOL <- coords.med.SOL %>% # filter(Latitude.rads <= 0) %>%
  mutate(area = (r^2) * (sin(Latitude.rads) - sin(Latitude.rads + d.lat.rads)) * (Longitude.rads - (Longitude.rads + d.long.rads))) %>% 
  mutate(area = ifelse(area < 0, abs(area), area)) # last value (Latitude 90), was negative, so had to take abs

sum_earth_sfarea <- sum(coords.med.SOL$area)
print(format(sum_earth_sfarea, scientific = TRUE)) #passed check of 5.1E14 m2

# Step 2. calculating deposition flux for each individual grid box, and convert to Gg/year
coords.med.SOL <- coords.med.SOL %>% mutate(budget_v1 = mean_flux.v1*area*60*60*24*365*(1e-6),
                                          budget_v2 = mean_flux.v2*area*60*60*24*365*(1e-6),
                                          budget_v3 = mean_flux.v3*area*60*60*24*365*(1e-6),
                                          budget_v4 = mean_flux.v4*area*60*60*24*365*(1e-6),
                                         # budget_v5 = mean_flux.v5*area*60*60*24*365*(1e-6),
                                          budget_v6 = mean_flux.v6*area*60*60*24*365*(1e-6),
                                          budget_v7 = mean_flux.v7*area*60*60*24*365*(1e-6),
                                          budget_v8 = mean_flux.v8*area*60*60*24*365*(1e-6))

total.SOL.budgets.2 <- coords.med.SOL %>% dplyr::select(budget_v1, budget_v2) %>% summarise(across(everything(), sum))

# Step 3. Compile regionally
coords.med.SOL$Latitude <- as.numeric(coords.med.SOL$Latitude)
coords.med.SOL$Longitude <- as.numeric(coords.med.SOL$Longitude)

coords.med.SOL <- coords.med.SOL %>% 
  mutate(ocean_region = case_when(
    (Longitude >= 295 & Latitude < -15 & Latitude > -45) | (Longitude <= 45 & Latitude < -15 & Latitude > -45) ~ "SATL",
    (Longitude >= 265 & Latitude > 30 & Latitude < 60) | (Longitude < 100 & Latitude > 30 & Latitude < 60) ~ "NATL",
    Longitude <= 78 & Longitude >= 30 & Latitude < 30 & Latitude > -15 ~ "Arabian Sea",
    Longitude <= 100 & Longitude >= 78 & Latitude < 30 & Latitude > -15 ~ "Bay of Bengal",
    Longitude < 150 & Longitude > 100 & Latitude <= 60 & Latitude > -15 ~ "SEAS",
    Longitude <= 265 & Longitude > 150 & Latitude < 60 & Latitude >= 30 ~ "NPAC",
    Latitude > 60 ~ "ARCT",
    Longitude < 295 & Longitude > 135 & Latitude <= -15 & Latitude >= -60 ~ "AUSP",
    Longitude > 45 & Longitude < 135 & Latitude < -15 & Latitude > -45 ~ "SIND",
    Longitude >= 0 & Latitude <= -45 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > -15 & Latitude < 30) | (Longitude < 30 & Latitude > -15 & Latitude < 30) ~ "CPAO")) 

na_count <- sum(is.na(coords.med.SOL$ocean_region))
print(na_count)
region_counts <- table(coords.med.SOL$ocean_region)
print(region_counts)

annual.SOL.budgets <- coords.med.SOL %>% dplyr::select(budget_v1, budget_v2, budget_v3, budget_v4, 
                                                       #budget_v5, 
                                                       budget_v6, budget_v7, budget_v8, ocean_region) %>% group_by(ocean_region) %>% summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>% filter(ocean_region != 'NA') %>% 
  mutate(percent_inc_v2 = ((budget_v2/budget_v1)*100)-100, percent_inc_v3 = ((budget_v3/budget_v2)*100)-100, percent_inc_v4 = ((budget_v4/budget_v3)*100)-100)


total.SOL.budgets <- annual.SOL.budgets %>% dplyr::select(-ocean_region) %>% summarise(across(everything(), sum)) %>% 
  mutate(percent_inc_v2 = ((budget_v2/budget_v1)*100)-100, percent_inc_v3 = ((budget_v3/budget_v2)*100)-100, percent_inc_v4 = ((budget_v4/budget_v3)*100)-100)

library(openxlsx)
write.xlsx(annual.SOL.budgets, file = "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\annual_soluble_iron_depo_budgets.xlsx")

# single day calculations ----------------------------------------------------------------------------------------
# important to eventually look at seasonality, etc. 
# Step 1: write function to calculate area of each grid box:
grid.areas <- expand.grid(all.lat.options, all.long.options)
names(grid.areas) <- c("Latitude", "Longitude")
grid.areas <- grid.areas %>% mutate(Latitude.rads = Latitude*(pi/180), Longitude.rads = Longitude*(pi/180))
r <- 6.371E6 # in m
d.lat.rads <- d.lat*(pi/180) # convert to radians
d.long.rads <- d.long*(pi/180) # convert to radians

grid.areas <- grid.areas %>%
  mutate(area = (r^2) * (sin(Latitude.rads) - sin(Latitude.rads + d.lat.rads)) * (Longitude.rads - (Longitude.rads + d.long.rads))) %>% 
  mutate(area = ifelse(area < 0, abs(area), area)) # last value (Latitude 90), was negative, so had to take abs

sum_earth_sfarea_tot <- (sum(grid.areas$area))
print(format(sum_earth_sfarea_tot, scientific = TRUE)) #passed check of 5.1E14 m2

# Step 2: assign indices for lon, lat, and time from large array 
# Define the range of indices for each dimension
Longitude.options <- as.data.frame(all.long.options)
Longitude.options <- Longitude.options %>% mutate(Longitude = all.long.options, Index = seq_along(Longitude)) %>% dplyr::select(-all.long.options)

Latitude.options <- as.data.frame(all.lat.options)
Latitude.options <- Latitude.options %>% mutate(Latitude = all.lat.options, Index = seq_along(Latitude)) %>% dplyr::select(-all.lat.options)
Latitude.options <- Latitude.options %>%  mutate(Latitude = round(Latitude, 2))

Time.options <- as.data.frame(all.time.options)
Time.options <- Time.options %>% mutate(Time = all.time.options, Index = seq_along(Time)) %>% dplyr::select(-all.time.options)
date_sequence <- as.data.frame(seq(as.Date("2009-01-01"), as.Date("2011-12-31"), by = "day"))
Time.options <- cbind(Time.options, date_sequence)

get_nearest_index <- function(value, options_df, column_name) {
  nearest_value <- options_df[[column_name]][which.min(abs(options_df[[column_name]] - value))]
  nearest_index <- options_df$Index[options_df[[column_name]] == nearest_value]
  return(nearest_index)
}

# Function to retrieve the iron deposition value
get_iron_deposition <- function(lon_deg, lat_deg, date, array) {
  # Retrieve indices from the options dataframes
  lon_index <- get_nearest_index(lon_deg, Longitude.options, "Longitude")
  lat_index <- get_nearest_index(lat_deg, Latitude.options, "Latitude")
  time_index <- Time.options$Index[Time.options$date == as.Date(date)]
  
  if (length(lon_index) == 0 || length(lat_index) == 0 || length(time_index) == 0) {
    stop("Invalid input: check if the specified longitude, latitude, and date exist in the options dataframes.")
  }
  
  # Extract the value from the array
  value <- array[lon_index, lat_index, time_index]
  
  # Find the corresponding grid area
  matching_area <- grid.areas$area[grid.areas$Latitude == round(lat_deg, 2) & grid.areas$Longitude == round(lon_deg, 2)]
  
  if (length(matching_area) == 0) {
    stop("No matching area found in grid.areas for the specified latitude and longitude.")
  }
  
  # Perform the calculations
  value <- value * 3600 * 24 * matching_area
  
  return(value)
}

# Example usage
iron_deposition <- get_iron_deposition(10, -87.17, "2010-06-15", FESOLDEP.v1)

value <- FESOLDEP.v1[1:288, 2:3,1] 
print(iron_deposition)
print(Latitude.options$Latitude)
