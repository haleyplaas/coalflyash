setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())
#.rs.restartR() # turn on when need to clear up RAM space

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc);library(terra)

#install.packages("")
# Reading in the data ----------------------------------------------------------------------- 
PI_sim_h1 <- nc_open("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33-FIRE33.cam.h1.2009-2011_PI.nc")

PD_sim_h1 <- nc_open("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38.cam.h1.2009-2011_PD.nc")

#FU_sim_h1 <- nc_open("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\")

# extracting deposition fluxes from files ----------------------------------------------------------------------
# creating vectors of all possible longitude, latitude, and timepoint options -- long, lat, and lev are the same for all of Douglas' coal-fly-ash simulations, doubled checked by reading them in individually, but TIME is different and 1D -- 920 options for v1-3, but 1640 options for v4. # I think this means I will just need to calculate my medians offline because the values is already a composite value calculated online

# looking at dimensions
# print(sapply(PI_sim_h1$var$FESOLDRY$dim, function(dim) dim$name))

all.lon.options <- ncvar_get(PI_sim_h1, "lon")
all.lat.options <- ncvar_get(PI_sim_h1, "lat")
all.lev.options <- ncvar_get(PI_sim_h1, "lev")
all.time.options <- ncvar_get(PI_sim_h1, "time")

#d.lat <- 180.0/length(all.lat.options)
#d.lon <- 360/length(all.lon.options)
d.lat <- abs(all.lat.options[2] - all.lat.options[1])  # Latitude grid spacing in degrees -- this is more accurate way to determine d_lat with bounding issues
d.lon <- abs(all.lon.options[2] - all.lon.options[1])  # Longitude grid spacing in degrees
d_time <- length(all.time.options)/365

# Extract the necessary variables
# PI simulation
var.SOL.DD.PI <- ncvar_get(PI_sim_h1, "FESOLDRY")
var.SOL.WD.PI <- ncvar_get(PI_sim_h1, "FESOLWET")
var.TOT.DD.PI <- ncvar_get(PI_sim_h1, "FETOTDRY")
var.TOT.WD.PI <- ncvar_get(PI_sim_h1, "FETOTWET")

var.ANSOL.DD.PI <- ncvar_get(PI_sim_h1, "FEANSOLDRY")
var.ANSOL.WD.PI <- ncvar_get(PI_sim_h1, "FEANSOLWET")
var.ANTOT.DD.PI <- ncvar_get(PI_sim_h1, "FEANTOTDRY")
var.ANTOT.WD.PI <- ncvar_get(PI_sim_h1, "FEANTOTWET")

# Filter the data to consider only the first level when lev > 1 (surface altitude) ----------------------------------------------
# XYZ <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface if needed
# if you plug in specific index values [1:288, 1:192, 1:1095] this works, blank just means selecting all 
SOL.DD.PI <- var.SOL.DD.PI[ , , ] 
SOL.WD.PI <- var.SOL.WD.PI[ , , ]
FESOLDEP.PI <- SOL.DD.PI + SOL.WD.PI # add dry and wet deposition together to get total deposition
# checked a couple indices manually to ensure that totaled emissions values in Panoply align with these dfs

TOT.DD.PI <- var.TOT.DD.PI[ , , ] 
TOT.WD.PI <- var.TOT.WD.PI[ , , ]
FETOTDEP.PI <- TOT.DD.PI + TOT.WD.PI

ANSOL.DD.PI <- var.ANSOL.DD.PI[ , , ] 
ANSOL.WD.PI <- var.ANSOL.WD.PI[ , , ]
FEANSOLDEP.PI <- ANSOL.DD.PI + ANSOL.WD.PI

ANTOT.DD.PI <- var.ANTOT.DD.PI[ , , ] 
ANTOT.WD.PI <- var.ANTOT.WD.PI[ , , ]
FEANTOTDEP.PI <- ANTOT.DD.PI + ANTOT.WD.PI

# Calculate the mean over entire time period -------------------------------------------------------------
# c(1, 2) indicates calculating over the first and second dimension (lon and lat)
mean.SOLDEP.PI <- apply(FESOLDEP.PI, c(1, 2), mean, na.rm = TRUE) 
mean.TOTDEP.PI <- apply(FETOTDEP.PI, c(1, 2), mean, na.rm = TRUE) 
mean.ANSOLDEP.PI <- apply(FEANSOLDEP.PI, c(1, 2), mean, na.rm = TRUE) 
mean.ANTOTDEP.PI <- apply(FEANTOTDEP.PI, c(1, 2), mean, na.rm = TRUE) 

nc_close(PI_sim_h1)

# transpose because lat and lons are flipped
mean.SOLDEP.PI <- as.data.frame(t(mean.SOLDEP.PI)) 
colnames(mean.SOLDEP.PI) <- all.lon.options  # Set column names to longitude values
rownames(mean.SOLDEP.PI) <- all.lat.options  # Set row names to latitude values

mean.TOTDEP.PI <- as.data.frame(t(mean.TOTDEP.PI)) 
colnames(mean.TOTDEP.PI) <- all.lon.options  # Set column names to longitude values
rownames(mean.TOTDEP.PI) <- all.lat.options  # Set row names to latitude values

mean.ANSOLDEP.PI <- as.data.frame(t(mean.ANSOLDEP.PI)) 
colnames(mean.ANSOLDEP.PI) <- all.lon.options  # Set column names to longitude values
rownames(mean.ANSOLDEP.PI) <- all.lat.options  # Set row names to latitude values

mean.ANTOTDEP.PI <- as.data.frame(t(mean.ANTOTDEP.PI)) 
colnames(mean.ANTOTDEP.PI) <- all.lon.options  # Set column names to longitude values
rownames(mean.ANTOTDEP.PI) <- all.lat.options  # Set row names to latitude values

mean.SOLDEP.PI$lat <- rownames(mean.SOLDEP.PI)
mean.SOLDEP.PI_long <- pivot_longer(
  mean.SOLDEP.PI,
  cols = -lat,
  names_to = "lon", 
  values_to = "Fe_emissions")

# Now onto calculating emission budgets
# Convert lat and lon to radians
lon_rads <- all.lon.options * pi / 180
lat_rads <- all.lat.options * pi / 180

# Calculate grid spacing
d_lat <- abs(all.lat.options[2] - all.lat.options[1])  # Latitude grid spacing in degrees
d_lon <- abs(all.lon.options[2] - all.lon.options[1])  # Longitude grid spacing in degrees
g_lat <- (d_lat / 2) * pi / 180  # Latitude half-spacing in radians
g_lon <- (d_lon / 2) * pi / 180  # Longitude half-spacing in radians

# Earth's radius
R <- 6.3781E6  

# Initialize an empty dataframe
cell_areas_df <- data.frame(lon = numeric(), lat = numeric(), cell_Area = numeric())

# Loop to calculate cell areas and directly append to the dataframe
for (i in seq_along(all.lat.options)) {
  for (j in seq_along(all.lon.options)) {
    # Convert latitude and longitude to radians
    lat_center <- lat_rads[i]
    lon_center <- lon_rads[j]
    
    # Compute staggered latitudes
    lat_north <- lat_center + g_lat
    lat_south <- lat_center - g_lat
    
    # Clip staggered latitudes to valid range
    lat_north <- pmax(pmin(lat_north, pi / 2), -pi / 2)
    lat_south <- pmax(pmin(lat_south, pi / 2), -pi / 2)
    
    # Compute area of the cell
    cell_area <- R^2 * (sin(lat_north) - sin(lat_south)) * (2 * g_lon)
    
    # Append the results to the dataframe
    cell_areas_df <- rbind(
      cell_areas_df, 
      data.frame(lon = all.lon.options[j], lat = all.lat.options[i], cell_Area = cell_area)
    )
  }
}

# Check if surface areas sum to Earth's total surface area (~5.1e14 m²)
sum_sa_earth <- sum(cell_areas_df$cell_Area, na.rm = TRUE)
cat(sprintf("Surface area: %3e m²\n", sum_sa_earth))

annual.conversion <- 3600 * 24 * 365 * 1e-9 # in Tg
monthly.conversion <- 3600 * 24 * 30 * 1e-9 # in Tg

# Convert lat and lon to numeric if needed
mean.SOLDEP.PI_long$lat <- as.numeric(mean.SOLDEP.PI_long$lat)
mean.SOLDEP.PI_long$lon <- as.numeric(mean.SOLDEP.PI_long$lon)

dif.lats <- mean.SOLDEP.PI_long$lat != cell_areas_df$lat
print(which(dif.lats == TRUE))
any(mean.SOLDEP.PI_long$lon != cell_areas_df$lon)

single_budget_calcs <- left_join(mean.SOLDEP.PI_long, cell_areas_df, by = c("lon", "lat"))# 

which(is.na(cell_areas_df$nearest_lat))

%>% 
  mutate(Fe_budg = (Fe_emissions*cell_Area*annual.conversion))





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


