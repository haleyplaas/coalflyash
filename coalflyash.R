setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())
#.rs.restartR() # turn on when need to clear up RAM space

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

#install.packages("")

# Read in observational data (excel file) with specified parameters -----------------------
data.file <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\FeObs_Hamilton2022.xlsx"

obs.data <- read_excel(data.file, sheet = "FeObs_Hamilton2021",
                 col_names = TRUE,
                 na = c("-99", "-9999", "-99.0","-0.99"))
                 
# function convert any longitude encoded as deg W to 360 E 
convert_longitude <- function(longitude) { #check integer vs real numbers to ensure nothing was rounded weird 
  if (longitude < 0.0) {
    return(360.0 + longitude)
  } else {
    return(longitude)
  }
}

# Convert Longitude column
obs.data$Longitude <- sapply(obs.data$Longitude, convert_longitude)
# print(obs.data$Longitude) # doubled checked with excel file to ensure 

# Read in the simulations data (netcdf files) ----------------------------------------------

# Function to generate array of values,condense to single median value over entire time period
## 
library(ncdf4);library(dplyr);library(tidyr)
generate_median_values_from_netcdf <- function(file_directory, var1_name, var2_name) {
  # Open the NetCDF file
  nc_data <- nc_open(file_directory)
  
  # Extract latitude and longitude options
  all_long_options <- ncvar_get(nc_data, "lon")
  all_lat_options <- ncvar_get(nc_data, "lat")
  
  # Extract the specified variables
  variable1 <- ncvar_get(nc_data, var1_name)
  variable2 <- ncvar_get(nc_data, var2_name)
  
  # Calculate median values for the specified variables
  median_var1 <- apply(variable1, c(1, 2), median, na.rm = TRUE)
  median_var2 <- apply(variable2, c(1, 2), median, na.rm = TRUE)
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Convert results to data frames with latitude and longitude columns
  median_var1_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    median_value = as.vector(median_var1)
  )
  
  median_var2_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    median_value = as.vector(median_var2)
  )
  
  # Reshape data frames to wide format
  coords_wide_var1 <- pivot_wider(median_var1_df, names_from = longitude, values_from = median_value)
  coords_wide_var2 <- pivot_wider(median_var2_df, names_from = longitude, values_from = median_value)
  
  # Convert tibbles to data frames and set row names
  coords_wide_var1 <- as.data.frame(coords_wide_var1)
  coords_wide_var2 <- as.data.frame(coords_wide_var2)
  rownames(coords_wide_var1) <- all_lat_options
  rownames(coords_wide_var2) <- all_lat_options
  coords_wide_var1 <- coords_wide_var1 %>% dplyr::select(-latitude)
  coords_wide_var2 <- coords_wide_var2 %>% dplyr::select(-latitude)
  
  # Return the wide data frames
  list(coords_wide_var1, coords_wide_var2)
}

result <- generate_median_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v1.1 <- result[[1]]
coords_wide.sol.med.v1.1 <- result[[2]]



#files for comparison for coal ash work
sim.1 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc"
sim.v1.PD <- nc_open(sim.1)
sim.2 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD10-OIL38.cam.h2.2009-2011_PD.nc"
sim.v2.PD <- nc_open(sim.2) #note, this was originally v2, but as of 5/28/2024 Douglas informed me that the variables in the title were switched; need to clarify with Douglas what I am comparing, fine after the re-runs are done
sim.3 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38.cam.h2.2009-2011_PD.nc"
sim.v3.PD <- nc_open(sim.3) 
sim.4 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25.cam.h2.2009-2011_PD.nc"
sim.v4.PD <- nc_open(sim.4)
sim.5 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25-FIREFINE56.cam.h2.2009-2011_PD.nc" #actually is resicoal 0.2
sim.v5.PD <- nc_open(sim.5)
sim.6 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE33.cam.h2.2009-2011_PI.nc"
sim.v1.PI <- nc_open(sim.6)
sim.7 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE56.cam.h2.2009-2011_PI.nc"
sim.v2.PI <- nc_open(sim.7)
sim.8 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33-FIRE33.cam.h2.2009-2011_PI.nc"
sim.v3.PI <- nc_open(sim.8)

# creating vectors of all possible longitude, latitude, and timepoint options -- long, lat, and lev are the same for all of Douglas' coal-fly-ash simulations, doubled checked by reading them in individually, but TIME is different and 1D -- 920 options for v1-3, but 1640 options for v4. # I think this means I will just need to calculate my medians offline because the values is already a composite value calculated online
all.long.options <- ncvar_get(sim.v1.PD, "lon") # PI and PD are the same
all.lat.options <- ncvar_get(sim.v1.PD, "lat") # PI and PD are the same
all.lev.options <- ncvar_get(sim.v1.PI, "lev") # PI and PD are the same
all.time.options <- ncvar_get(sim.v1.PI, "time") # PI and PD are the same
all.time.options.PI.v3 <- ncvar_get(sim.v5.PD, "time") # PI and PD are the same other than v3.PI, which is doubled

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- 180.0/length(all.lat.options)
d.long <- 360/length(all.long.options)
d.time <- 365/length(all.time.options)
d.time.PI.v3 <- 365/length(all.time.options.PI.v3)

# Convert netcdf array to dataframe and calculate averages at specified resolution -- this passed cross examination in Panoply and using single-point extractions averaged over time period specified(tried annual and single-day) ----------- 
# Extract the necessary variable -- base 
#variable <- ncvar_get(model.data.x, "ncl_a1")
#variable <- ncvar_get(model.data.x, "FETOTSRF")
#variable.sol <- ncvar_get(model.data.x, "FESOLSRF")

# PD.v1 INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38
variable.v1 <- ncvar_get(sim.v1.PD, "FETOTSRF")
variable.sol.v1 <- ncvar_get(sim.v1.PD, "FESOLSRF")

# PD.v2 INDCOAL0.2-RESICOAL33-WOOD10-OIL38
variable.v2 <- ncvar_get(sim.v2.PD, "FETOTSRF")
variable.sol.v2 <- ncvar_get(sim.v2.PD, "FESOLSRF")

# PD.v3 INDCOAL0.2-RESICOAL33-WOOD56-OIL38
variable.v3 <- ncvar_get(sim.v3.PD, "FETOTSRF")
variable.sol.v3 <- ncvar_get(sim.v3.PD, "FESOLSRF")

# PD.v4 INDCOAL0.05-RESICOAL33-WOOD56-OIL25
variable.v4 <- ncvar_get(sim.v4.PD, "FETOTSRF")
variable.sol.v4 <- ncvar_get(sim.v4.PD, "FESOLSRF")

# PD.v5 INDCOAL0.05-RESICOAL0.2-WOOD56-OIL25-FIREFINE56 # accurately reflects values now
variable.v5 <- ncvar_get(sim.v5.PD, "FETOTSRF")
variable.sol.v5 <- ncvar_get(sim.v5.PD, "FESOLSRF")

# PI MIMI SIMULATION = PI.v1 RESICOAL0.2-FIRE33
variable.PI.v1 <- ncvar_get(sim.v1.PI, "FETOTSRF")
variable.sol.PI.v1 <- ncvar_get(sim.v1.PI, "FESOLSRF")

# PI.v2 RESICOAL0.2-FIRE56
variable.PI.v2 <- ncvar_get(sim.v2.PI, "FETOTSRF")
variable.sol.PI.v2 <- ncvar_get(sim.v2.PI, "FESOLSRF")

# PI.v3 RESICOAL33-FIRE33
variable.PI.v3 <- ncvar_get(sim.v3.PI, "FETOTSRF")
variable.sol.PI.v3 <- ncvar_get(sim.v3.PI, "FESOLSRF")

# only run this if you need to see the dimensions to double check them 
dim(variable.v3)

# Filter the data to consider only the first level when lev > 1 (surface altitude) 
# ncl_a1 <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface
# base code
#FETOTSRF <- variable[ , , ] #this is where to specify specific grid boxes or days (longitude, latitude, time) per 3 dimensions, use : to select a range 
#FESOLSRF <- variable.sol[ , , ]

# For Mingjin's coal-fly-ash simulations 
FETOTSRF.v1 <- variable.v1[ , , ] 
FESOLSRF.v1 <- variable.sol.v1[ , , ]

FETOTSRF.v2 <- variable.v2[ , , ]
FESOLSRF.v2 <- variable.sol.v2[ , , ]

FETOTSRF.v3 <- variable.v3[ , , ]
FESOLSRF.v3 <- variable.sol.v3[ , , ]

FETOTSRF.v4 <- variable.v4[ , , ] 
FESOLSRF.v4 <- variable.sol.v4[ , , ]

FETOTSRF.v5 <- variable.v5[ , , ] 
FESOLSRF.v5 <- variable.sol.v5[ , , ]

# PRE INDUSTRIAL SIMULATIONS -- 3 years of data -- has 56 levels
FETOTSRF.PI.v1 <- variable.PI.v1[ , , ] 
FESOLSRF.PI.v1 <- variable.sol.PI.v1[ , , ]

FETOTSRF.PI.v2 <- variable.PI.v2[ , , ] 
FESOLSRF.PI.v2 <- variable.sol.PI.v2[ , , ]

FETOTSRF.PI.v3 <- variable.PI.v3[ , , ] 
FESOLSRF.PI.v3 <- variable.sol.PI.v3[ , , ]

# Calculate the annual average and annual median -- median is preferred but examine differences between the two 
#annual_average <- apply(FETOTSRF, c(1, 2), mean, na.rm = TRUE)  # Calculate mean or median over time dimension 
#annual_average.sol <- apply(FESOLSRF, c(1, 2), mean, na.rm = TRUE) 
#annual_median <- apply(FETOTSRF, c(1, 2), median, na.rm = TRUE)
#annual_median.sol <- apply(FESOLSRF, c(1, 2), median, na.rm = TRUE)

# PD.v1 
annual_average.v1 <- apply(FETOTSRF.v1, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.v1 <- apply(FESOLSRF.v1, c(1, 2), mean, na.rm = TRUE) 
annual_median.v1 <- apply(FETOTSRF.v1, c(1, 2), median, na.rm = TRUE)
annual_median.sol.v1 <- apply(FESOLSRF.v1, c(1, 2), median, na.rm = TRUE)

# PD.v2 
annual_average.v2 <- apply(FETOTSRF.v2, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.v2 <- apply(FESOLSRF.v2, c(1, 2), mean, na.rm = TRUE) 
annual_median.v2 <- apply(FETOTSRF.v2, c(1, 2), median, na.rm = TRUE)
annual_median.sol.v2 <- apply(FESOLSRF.v2, c(1, 2), median, na.rm = TRUE)

# PD.v3 
annual_average.v3 <- apply(FETOTSRF.v3, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.v3 <- apply(FESOLSRF.v3, c(1, 2), mean, na.rm = TRUE) 
annual_median.v3 <- apply(FETOTSRF.v3, c(1, 2), median, na.rm = TRUE)
annual_median.sol.v3 <- apply(FESOLSRF.v3, c(1, 2), median, na.rm = TRUE)

# PD.v4 
annual_average.v4 <- apply(FETOTSRF.v4, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.v4 <- apply(FESOLSRF.v4, c(1, 2), mean, na.rm = TRUE) 
annual_median.v4 <- apply(FETOTSRF.v4, c(1, 2), median, na.rm = TRUE)
annual_median.sol.v4 <- apply(FESOLSRF.v4, c(1, 2), median, na.rm = TRUE)

# PD.v5 
annual_average.v5 <- apply(FETOTSRF.v5, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.v5 <- apply(FESOLSRF.v5, c(1, 2), mean, na.rm = TRUE) 
annual_median.v5 <- apply(FETOTSRF.v5, c(1, 2), median, na.rm = TRUE)
annual_median.sol.v5 <- apply(FESOLSRF.v5, c(1, 2), median, na.rm = TRUE)

#PI v1 (MIMI) 
annual_average.PI.v1 <- apply(FETOTSRF.PI.v1, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.PI.v1 <- apply(FESOLSRF.PI.v1, c(1, 2), mean, na.rm = TRUE) 
annual_median.PI.v1 <- apply(FETOTSRF.PI.v1, c(1, 2), median, na.rm = TRUE)
annual_median.sol.PI.v1 <- apply(FESOLSRF.PI.v1, c(1, 2), median, na.rm = TRUE)

#PI v2  
annual_average.PI.v2 <- apply(FETOTSRF.PI.v2, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.PI.v2 <- apply(FESOLSRF.PI.v2, c(1, 2), mean, na.rm = TRUE) 
annual_median.PI.v2 <- apply(FETOTSRF.PI.v2, c(1, 2), median, na.rm = TRUE)
annual_median.sol.PI.v2 <- apply(FESOLSRF.PI.v2, c(1, 2), median, na.rm = TRUE)

#PI v3 
annual_average.PI.v3 <- apply(FETOTSRF.PI.v3, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.PI.v3 <- apply(FESOLSRF.PI.v3, c(1, 2), mean, na.rm = TRUE) 
annual_median.PI.v3 <- apply(FETOTSRF.PI.v3, c(1, 2), median, na.rm = TRUE)
annual_median.sol.PI.v3 <- apply(FESOLSRF.PI.v3, c(1, 2), median, na.rm = TRUE)

# Closing netcdf files to free up space ----------------------------
nc_close(sim.v1.PD)
nc_close(sim.v2.PD)
nc_close(sim.v3.PD)
nc_close(sim.v4.PD)
nc_close(sim.v5.PD)
nc_close(sim.v1.PI)
nc_close(sim.v2.PI)
nc_close(sim.v3.PI)

# assigning model values to compile (spatially) for each observational data point -------------------
# at the grid-box resolution (1 grid box, n=1)
# recall, in the araka lamb "c" gridding approach, indicates a range, NOT a cross-sectional point:(0degE, 89.1degN) = (0-1.25degE and 89.1-90degN)
find_model_latitude.n1 <- function(df, all_lat_options) { # this was double checked against the old function (n=4) and passed the QA/QC
  df <- df %>%
    mutate(Model_Latitude_Values = sapply(Latitude, function(lat) {
      closest_lat <- all_lat_options[which.min(abs(all_lat_options - lat))]
      closest_lat
    }))
  return(df)
}
obs.data.n1 <- find_model_latitude.n1(obs.data, all.lat.options)

find_model_longitude.n1 <- function(df, all_lon_options) { # this was double checked against the old function (n=4) and passed the QA/QC
  df <- df %>%
    mutate(Model_Longitude_Values = sapply(Longitude, function(lon) {
      closest_lon <- all_lon_options[which.min(abs(all_lon_options - lon))]
      closest_lon
    }))
  return(df)
}
obs.data.mod.res.n1 <- find_model_longitude.n1(obs.data.n1, all.long.options)

# Function to collating observational data which fall in the same grid boxes 
process_data <- function(df) {
  df <- df %>%
    # Convert latitude and longitude columns to character type
    mutate(Model_Latitude_Values = as.character(Model_Latitude_Values),
           Model_Longitude_Values = as.character(Model_Longitude_Values)) %>%
    # Group by latitude and longitude values
    group_by(Model_Latitude_Values, Model_Longitude_Values) %>%
    # Calculate median for Fe (ng m-3), Fe solubility, and labile Fe (ng m-3)
    summarise(across(c("Fe (ng m–3)", "Fe solubility", "labile Fe (ng m–3)"), ~median(., na.rm = TRUE)), .groups = 'drop') %>%
    # Reset grouping
    ungroup() %>%
    # Convert latitude and longitude columns back to original type
    dplyr::mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values),
                  Model_Longitude_Values = as.numeric(Model_Longitude_Values))
  
  return(df)
}

process_data <- function(df) {
  df <- df %>%
    # Convert latitude and longitude columns to character type
    mutate(Model_Latitude_Values = as.character(Model_Latitude_Values),
           Model_Longitude_Values = as.character(Model_Longitude_Values)) %>%
    # Group by latitude and longitude values
    group_by(Model_Latitude_Values, Model_Longitude_Values) %>%
    # Calculate median and standard deviation for Fe (ng m-3), Fe solubility, and labile Fe (ng m-3)
    summarise(across(c("Fe (ng m–3)", "Fe solubility", "labile Fe (ng m–3)"), 
                     list(median = median, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
    # Reset grouping
    ungroup() %>%
    # Convert latitude and longitude columns back to original type
    mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values),
           Model_Longitude_Values = as.numeric(Model_Longitude_Values))
  
  return(df)
}

# Calculate median values for observational data points at same locations over time
obs.data.mod.res.n1 <- process_data(obs.data.mod.res.n1) # can turn on or off if wanting to view observational data as collated

# at 1/4 the model resolution (4 grid boxes, n=4)
find_model_latitude.n4 <- function(df, all_lat_options) {
  df <- df %>%
    mutate(Model_Latitude_Values = sapply(Latitude, function(lat) {
      matching_indices <- which(abs(all_lat_options - lat) <= d.lat) #change from value to dlat 
      if (length(matching_indices) > 0) {
        paste(all_lat_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  return(df)
}
obs.data.n4 <- find_model_latitude.n4(obs.data, all.lat.options)

find_model_longitude.n4 <- function(df, all_long_options) {
  df <- df %>%
    mutate(Model_Longitude_Values = sapply(Longitude, function(long) {
      matching_indices <- which(abs(all_long_options - long) <= d.long)
      if (length(matching_indices) > 0) {
        paste(all_long_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  return(df)
}
obs.data.mod.res.n4 <- find_model_longitude.n4(obs.data.n4, all.long.options)


# Create a dataframe with latitude, longitude, and annual AVERAGE # this is the base code ---------------------------------
#coords <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)), longitude = rep(all.long.options, length(all.lat.options)), annual_average = as.vector(annual_average)) 
#coords_wide <- pivot_wider(coords, names_from = longitude, values_from = annual_average)
#coords_wide <- spread(coords, key = longitude, value = annual_average) 
#rownames(coords_wide) <- all.lat.options 
#coords_wide <- coords_wide[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
#coords.sol <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),longitude = rep(all.long.options, length(all.lat.options)),annual_average.sol = as.vector(annual_average.sol))
#coords_wide.sol <- pivot_wider(coords.sol, names_from = longitude, values_from = annual_average.sol)
#coords_wide.sol <- spread(coords.sol, key = longitude, value = annual_average.sol) 
#rownames(coords_wide.sol) <- all.lat.options 
#coords_wide.sol <- coords_wide.sol[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# Create a dataframe with latitude, longitude, and annual MEDIAN
#coords.med <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)), longitude = rep(all.long.options, length(all.lat.options)), annual_median = as.vector(annual_median)) 
#coords_wide.med <- pivot_wider(coords.med, names_from = longitude, values_from = annual_median)
#coords_wide.med <- spread(coords.med, key = longitude, value = annual_median) 
#rownames(coords_wide.med) <- all.lat.options 
#coords_wide.med <- coords_wide.med[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
#coords.sol.med <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)), longitude = rep(all.long.options, length(all.lat.options)), annual_median.sol = as.vector(annual_median.sol))
#coords_wide.sol.med <- pivot_wider(coords.sol.med, names_from = longitude, values_from = annual_median.sol)
#coords_wide.sol.med <- spread(coords.sol.med, key = longitude, value = annual_median.sol) 
#rownames(coords_wide.sol.med) <- all.lat.options 
#coords_wide.sol.med <- coords_wide.sol.med[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# Create a dataframe with latitude, longitude, and median values for Fe (compiled over time) ------------------------
coords.med.v1 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_median.v1 = as.vector(annual_median.v1)) 
coords_wide.med.v1 <- pivot_wider(coords.med.v1, names_from = longitude, values_from = annual_median.v1)
coords_wide.med.v1 <- spread(coords.med.v1, key = longitude, value = annual_median.v1) 
rownames(coords_wide.med.v1) <- all.lat.options 
coords_wide.med.v1 <- coords_wide.med.v1[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.med.v1 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                  longitude = rep(all.long.options, length(all.lat.options)),
                                  annual_median.sol.v1 = as.vector(annual_median.sol.v1))
coords_wide.sol.med.v1 <- pivot_wider(coords.sol.med.v1, names_from = longitude, values_from = annual_median.sol.v1)
coords_wide.sol.med.v1 <- spread(coords.sol.med.v1, key = longitude, value = annual_median.sol.v1) 
rownames(coords_wide.sol.med.v1) <- all.lat.options 
coords_wide.sol.med.v1 <- coords_wide.sol.med.v1[,-1] # can only check in Panoply when single values 

# Create a dataframe with latitude, longitude, and annual MEDIAN -- calculated online # v2 --------------------------
coords.med.v2 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                            longitude = rep(all.long.options, length(all.lat.options)),
                            annual_median.v2 = as.vector(annual_median.v2)) 
coords_wide.med.v2 <- pivot_wider(coords.med.v2, names_from = longitude, values_from = annual_median.v2)
coords_wide.med.v2 <- spread(coords.med.v2, key = longitude, value = annual_median.v2) 
rownames(coords_wide.med.v2) <- all.lat.options 
coords_wide.med.v2 <- coords_wide.med.v2[,-1] 
# and specifying for soluble iron (averaged)
coords.sol.med.v2 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                longitude = rep(all.long.options, length(all.lat.options)),
                                annual_median.sol.v2 = as.vector(annual_median.sol.v2))
coords_wide.sol.med.v2 <- pivot_wider(coords.sol.med.v2, names_from = longitude, values_from = annual_median.sol.v2)
coords_wide.sol.med.v2 <- spread(coords.sol.med.v2, key = longitude, value = annual_median.sol.v2) 
rownames(coords_wide.sol.med.v2) <- all.lat.options 
coords_wide.sol.med.v2 <- coords_wide.sol.med.v2[,-1] 

# Create a dataframe with latitude, longitude, and annual MEDIAN (?) -- calculated online # v3 --------------------------
coords.med.v3 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                            longitude = rep(all.long.options, length(all.lat.options)),
                            annual_median.v3 = as.vector(annual_median.v3)) 
coords_wide.med.v3 <- pivot_wider(coords.med.v3, names_from = longitude, values_from = annual_median.v3)
coords_wide.med.v3 <- spread(coords.med.v3, key = longitude, value = annual_median.v3) 
rownames(coords_wide.med.v3) <- all.lat.options 
coords_wide.med.v3 <- coords_wide.med.v3[,-1]  
# and specifying for soluble iron (averaged)
coords.sol.med.v3 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                longitude = rep(all.long.options, length(all.lat.options)),
                                annual_median.sol.v3 = as.vector(annual_median.sol.v3))
coords_wide.sol.med.v3 <- pivot_wider(coords.sol.med.v3, names_from = longitude, values_from = annual_median.sol.v3)
coords_wide.sol.med.v3 <- spread(coords.sol.med.v3, key = longitude, value = annual_median.sol.v3) 
rownames(coords_wide.sol.med.v3) <- all.lat.options 
coords_wide.sol.med.v3 <- coords_wide.sol.med.v3[,-1]

# Create a dataframe with latitude, longitude, and annual MEDIAN (?) -- calculated online # v4 --------------------------
coords.med.v4 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                            longitude = rep(all.long.options, length(all.lat.options)),
                            annual_median.v4 = as.vector(annual_median.v4)) 
coords_wide.med.v4 <- pivot_wider(coords.med.v4, names_from = longitude, values_from = annual_median.v4)
coords_wide.med.v4 <- spread(coords.med.v4, key = longitude, value = annual_median.v4) 
rownames(coords_wide.med.v4) <- all.lat.options 
coords_wide.med.v4 <- coords_wide.med.v4[,-1]  
# and specifying for soluble iron (averaged)
coords.sol.med.v4 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                longitude = rep(all.long.options, length(all.lat.options)),
                                annual_median.sol.v4 = as.vector(annual_median.sol.v4))
coords_wide.sol.med.v4 <- pivot_wider(coords.sol.med.v4, names_from = longitude, values_from = annual_median.sol.v4)
coords_wide.sol.med.v4 <- spread(coords.sol.med.v4, key = longitude, value = annual_median.sol.v4) 
rownames(coords_wide.sol.med.v4) <- all.lat.options 
coords_wide.sol.med.v4 <- coords_wide.sol.med.v4[,-1] # passed Panoply check 

# Create a dataframe with latitude, longitude, and annual MEDIAN (?) -- calculated online # v5 --------------------------
coords.med.v5 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                            longitude = rep(all.long.options, length(all.lat.options)),
                            annual_median.v5 = as.vector(annual_median.v5)) 
coords_wide.med.v5 <- pivot_wider(coords.med.v5, names_from = longitude, values_from = annual_median.v5)
coords_wide.med.v5 <- spread(coords.med.v5, key = longitude, value = annual_median.v5) 
rownames(coords_wide.med.v5) <- all.lat.options 
coords_wide.med.v5 <- coords_wide.med.v5[,-1]  
# and specifying for soluble iron (averaged)
coords.sol.med.v5 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                longitude = rep(all.long.options, length(all.lat.options)),
                                annual_median.sol.v5 = as.vector(annual_median.sol.v5))
coords_wide.sol.med.v5 <- pivot_wider(coords.sol.med.v5, names_from = longitude, values_from = annual_median.sol.v5)
coords_wide.sol.med.v5 <- spread(coords.sol.med.v5, key = longitude, value = annual_median.sol.v5) 
rownames(coords_wide.sol.med.v5) <- all.lat.options 
coords_wide.sol.med.v5 <- coords_wide.sol.med.v5[,-1] # passed Panoply check 

# Create a dataframe with latitude, longitude, and annual MEDIAN for PI SIMULATION MIMI -----------------------
coords.med.PI.v1 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_median.PI.v1 = as.vector(annual_median.PI.v1)) 
coords_wide.med.PI.v1 <- pivot_wider(coords.med.PI.v1, names_from = longitude, values_from = annual_median.PI.v1)
coords_wide.med.PI.v1 <- spread(coords.med.PI.v1, key = longitude, value = annual_median.PI.v1) 
rownames(coords_wide.med.PI.v1) <- all.lat.options 
coords_wide.med.PI.v1 <- coords_wide.med.PI.v1[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.med.PI.v1 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                  longitude = rep(all.long.options, length(all.lat.options)),
                                  annual_median.sol.PI.v1 = as.vector(annual_median.sol.PI.v1))
coords_wide.sol.med.PI.v1 <- pivot_wider(coords.sol.med.PI.v1, names_from = longitude, values_from = annual_median.sol.PI.v1)
coords_wide.sol.med.PI.v1 <- spread(coords.sol.med.PI.v1, key = longitude, value = annual_median.sol.PI.v1) 
rownames(coords_wide.sol.med.PI.v1) <- all.lat.options 
coords_wide.sol.med.PI.v1 <- coords_wide.sol.med.PI.v1[,-1] 

# Create a dataframe with latitude, longitude, and annual MEDIAN for PI SIMULATION v2 -----------------------
coords.med.PI.v2 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_median.PI.v2 = as.vector(annual_median.PI.v2)) 
coords_wide.med.PI.v2 <- pivot_wider(coords.med.PI.v2, names_from = longitude, values_from = annual_median.PI.v2)
coords_wide.med.PI.v2 <- spread(coords.med.PI.v2, key = longitude, value = annual_median.PI.v2) 
rownames(coords_wide.med.PI.v2) <- all.lat.options 
coords_wide.med.PI.v2 <- coords_wide.med.PI.v2[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.med.PI.v2 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                  longitude = rep(all.long.options, length(all.lat.options)),
                                  annual_median.sol.PI.v2 = as.vector(annual_median.sol.PI.v2))
coords_wide.sol.med.PI.v2 <- pivot_wider(coords.sol.med.PI.v2, names_from = longitude, values_from = annual_median.sol.PI.v2)
coords_wide.sol.med.PI.v2 <- spread(coords.sol.med.PI.v2, key = longitude, value = annual_median.sol.PI.v2) 
rownames(coords_wide.sol.med.PI.v2) <- all.lat.options 
coords_wide.sol.med.PI.v2 <- coords_wide.sol.med.PI.v2[,-1] 

# Create a dataframe with latitude, longitude, and annual MEDIAN for PI SIMULATION v3 -----------------------
coords.med.PI.v3 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_median.PI.v3 = as.vector(annual_median.PI.v3)) 
coords_wide.med.PI.v3 <- pivot_wider(coords.med.PI.v3, names_from = longitude, values_from = annual_median.PI.v3)
coords_wide.med.PI.v3 <- spread(coords.med.PI.v3, key = longitude, value = annual_median.PI.v3) 
rownames(coords_wide.med.PI.v3) <- all.lat.options 
coords_wide.med.PI.v3 <- coords_wide.med.PI.v3[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.med.PI.v3 <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                  longitude = rep(all.long.options, length(all.lat.options)),
                                  annual_median.sol.PI.v3 = as.vector(annual_median.sol.PI.v3))
coords_wide.sol.med.PI.v3 <- pivot_wider(coords.sol.med.PI.v3, names_from = longitude, values_from = annual_median.sol.PI.v3)
coords_wide.sol.med.PI.v3 <- spread(coords.sol.med.PI.v3, key = longitude, value = annual_median.sol.PI.v3) 
rownames(coords_wide.sol.med.PI.v3) <- all.lat.options 
coords_wide.sol.med.PI.v3 <- coords_wide.sol.med.PI.v3[,-1] 

# For loop to compile obs with models values -- for coal fly ash v1  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
v1_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR v1 
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.v1[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v1_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v1_values <- v1_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
v1_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.v1[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v1_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v1_values.sol<- v1_values.sol

# creating new dataframe just with the important information 
# obs.mod.comparison.n1 <- obs.data.mod.res.n1 %>% dplyr::select(Latitude, Longitude, `Fe (ng m–3)`,`Fe solubility`,`labile Fe (ng m–3)`, mod_values, mod_values.sol) %>% rename(Obs_Fe = `Fe (ng m–3)`, Mod_Fe = `mod_values`, Obs_sol_Fe = `labile Fe (ng m–3)`, Mod_sol_Fe = `mod_values.sol`, Obs_solubility = `Fe solubility`) %>% mutate(Mod_solubility = Mod_sol_Fe / Mod_Fe) # use this when not compiling observations by grid
obs.mod.comparison.v1 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, v1_values, v1_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, v1_Fe = `v1_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, v1_sol_Fe = `v1_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(v1_solubility = v1_sol_Fe / v1_Fe)

# For loop to compile obs with models values -- for v2  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
v2_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR v2 
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.v2[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v2_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v2_values <- v2_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
v2_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.v2[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v2_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v2_values.sol<- v2_values.sol

# creating new dataframe just with the important information 
obs.mod.comparison.v2 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, v2_values, v2_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, v2_Fe = `v2_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, v2_sol_Fe = `v2_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(v2_solubility = v2_sol_Fe / v2_Fe)

# For loop to compile obs with models values -- for v3  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
v3_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR v3 
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.v3[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v3_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v3_values <- v3_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
v3_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.v3[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v3_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v3_values.sol<- v3_values.sol

# creating new dataframe just with the important information 
obs.mod.comparison.v3 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, v3_values, v3_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, v3_Fe = `v3_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, v3_sol_Fe = `v3_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(v3_solubility = v3_sol_Fe / v3_Fe)

# For loop to compile obs with models values -- for v4  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
v4_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR v4 
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.v4[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v4_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v4_values <- v4_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
v4_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.v4[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v4_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v4_values.sol<- v4_values.sol

# creating new dataframe just with the important information 
obs.mod.comparison.v4 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, v4_values, v4_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, v4_Fe = `v4_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, v4_sol_Fe = `v4_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(v4_solubility = v4_sol_Fe / v4_Fe)

# For loop to compile obs with models values -- for v5  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
v5_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR v5 
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.v5[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v5_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v5_values <- v5_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
v5_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.v5[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  v5_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$v5_values.sol<- v5_values.sol

# creating new dataframe just with the important information 
obs.mod.comparison.v5 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, v5_values, v5_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, v5_Fe = `v5_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, v5_sol_Fe = `v5_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(v5_solubility = v5_sol_Fe / v5_Fe)
# For loop to compile obs with models values -- for PI v1  ---------------------------------------------------- 
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
PI.v1_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR PI v1
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.PI.v1[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  PI.v1_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$PI.v1_values <- PI.v1_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
PI.v1_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.PI.v1[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  PI.v1_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$PI.v1_values.sol<- PI.v1_values.sol

# creating new dataframe just with the important information 
obs.mod.comparison.PI.v1 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, PI.v1_values, PI.v1_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, PI.v1_Fe = `PI.v1_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, PI.v1_sol_Fe = `PI.v1_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(PI.v1_solubility = PI.v1_sol_Fe / PI.v1_Fe)

# For loop to compile obs with models values -- for PI v2  ---------------------------------------------------- 
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
PI.v2_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR PI v2
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.PI.v2[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  PI.v2_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$PI.v2_values <- PI.v2_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
PI.v2_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.PI.v2[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  PI.v2_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$PI.v2_values.sol<- PI.v2_values.sol

# creating new dataframe just with the important information 
# obs.mod.comparison.n1 <- obs.data.mod.res.n1 %>% dplyr::select(Latitude, Longitude, `Fe (ng m–3)`,`Fe solubility`,`labile Fe (ng m–3)`, mod_values, mod_values.sol) %>% rename(Obs_Fe = `Fe (ng m–3)`, Mod_Fe = `mod_values`, Obs_sol_Fe = `labile Fe (ng m–3)`, Mod_sol_Fe = `mod_values.sol`, Obs_solubility = `Fe solubility`) %>% mutate(Mod_solubility = Mod_sol_Fe / Mod_Fe) # use this when not compiling observations by grid
obs.mod.comparison.PI.v2 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, PI.v2_values, PI.v2_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, PI.v2_Fe = `PI.v2_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, PI.v2_sol_Fe = `PI.v2_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(PI.v2_solubility = PI.v2_sol_Fe / PI.v2_Fe)
# For loop to compile obs with models values -- for PI v3  ---------------------------------------------------- 
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
PI.v3_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR PI v3
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.med.PI.v3[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  PI.v3_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$PI.v3_values <- PI.v3_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
PI.v3_values.sol <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res.n1))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res.n1$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res.n1$Model_Longitude_Values[[row_index]]
  
  # Initialize an empty vector to store the extracted values for the current row
  extracted_values <- numeric(length(model_latitude_values))
  
  # Loop over each latitude and longitude pair
  for (i in seq_along(model_latitude_values)) {
    # Extract latitude and longitude values for the current pair
    lat <- as.numeric(unlist(strsplit(model_latitude_values[i], ", ")))
    lon <- as.numeric(unlist(strsplit(model_longitude_values[i], ", ")))
    
    # Extract the value from coords_wide.med based on the latitude and longitude
    extracted_value <- coords_wide.sol.med.PI.v3[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  PI.v3_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$PI.v3_values.sol<- PI.v3_values.sol

# creating new dataframe just with the important information 
# obs.mod.comparison.n1 <- obs.data.mod.res.n1 %>% dplyr::select(Latitude, Longitude, `Fe (ng m–3)`,`Fe solubility`,`labile Fe (ng m–3)`, mod_values, mod_values.sol) %>% rename(Obs_Fe = `Fe (ng m–3)`, Mod_Fe = `mod_values`, Obs_sol_Fe = `labile Fe (ng m–3)`, Mod_sol_Fe = `mod_values.sol`, Obs_solubility = `Fe solubility`) %>% mutate(Mod_solubility = Mod_sol_Fe / Mod_Fe) # use this when not compiling observations by grid
obs.mod.comparison.PI.v3 <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, PI.v3_values, PI.v3_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, PI.v3_Fe = `PI.v3_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, PI.v3_sol_Fe = `PI.v3_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(PI.v3_solubility = PI.v3_sol_Fe / PI.v3_Fe)

# merging all coal fly ash dataframes for easier comparisons  --------------------------------
coal.ash.sims <- cbind(obs.mod.comparison.v1, obs.mod.comparison.v2, obs.mod.comparison.v3, obs.mod.comparison.v4, obs.mod.comparison.v5, obs.mod.comparison.PI.v1, obs.mod.comparison.PI.v2,obs.mod.comparison.PI.v3)
coal.ash.sims <- coal.ash.sims[, !duplicated(colnames(coal.ash.sims))]

# adding geographical regions by latitude and longitude ------------------------------------------
coal.ash.sims$Latitude <- as.numeric(coal.ash.sims$Latitude)
coal.ash.sims$Longitude <- as.numeric(coal.ash.sims$Longitude)

coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_1 = case_when(
    (Longitude >= 295 & Latitude < -15 & Latitude > -60) | (Longitude <= 45 & Latitude < -15 & Latitude > -60) ~ "SATL",
    (Longitude >= 265 & Latitude > 30 & Latitude < 60) | (Longitude < 135 & Latitude > 30 & Latitude < 60) ~ "NATL",
    Longitude <= 135 & Longitude >= 35 & Latitude < 30 & Latitude > -15 ~ "NIND",
    Longitude < 150 & Longitude > 135 & Latitude <= 60 & Latitude > -15 ~ "SEAS",
    Longitude <= 265 & Longitude > 150 & Latitude < 60 & Latitude >= 30 ~ "NPAC",
    Longitude >= 0 & Latitude >= 60 & Latitude <= 90 ~ "ARCT",
    Longitude < 295 & Longitude > 135 & Latitude <= -15 & Latitude >= -60 ~ "AUSP",
    Longitude > 45 & Longitude < 135 & Latitude < -15 & Latitude > -60 ~ "SIND",
    Longitude >= 0 & Latitude <= -60 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > -15 & Latitude < 30) | (Longitude < 30 & Latitude > -15 & Latitude < 30) ~ "CPAO"))

na_count <- sum(is.na(coal.ash.sims$region_1))
print(na_count)
region_counts <- table(coal.ash.sims$region_1)
print(region_counts)

coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_2 = case_when(
    (Longitude >= 295 & Latitude < -15 & Latitude > -45) | (Longitude <= 45 & Latitude < -15 & Latitude > -45) ~ "SATL",
    (Longitude >= 265 & Latitude > 30 & Latitude < 60) | (Longitude < 100 & Latitude > 30 & Latitude < 60) ~ "NATL",
    Longitude <= 100 & Longitude >= 30 & Latitude < 30 & Latitude > -15 ~ "NIND",
    Longitude < 150 & Longitude > 100 & Latitude <= 60 & Latitude > -15 ~ "SEAS",
    Longitude <= 265 & Longitude > 150 & Latitude < 60 & Latitude >= 30 ~ "NPAC",
    Longitude >= 0 & Latitude >= 60 & Latitude <= 90 ~ "ARCT",
    Longitude < 295 & Longitude > 135 & Latitude <= -15 & Latitude >= -60 ~ "AUSP",
    Longitude > 45 & Longitude < 135 & Latitude < -15 & Latitude > -45 ~ "SIND",
    Longitude >= 0 & Latitude <= -45 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > -15 & Latitude < 30) | (Longitude < 30 & Latitude > -15 & Latitude < 30) ~ "CPAO"))

na_count <- sum(is.na(coal.ash.sims$region_2))
print(na_count)
region_counts <- table(coal.ash.sims$region_2)
print(region_counts)

coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_3 = case_when(
    (Longitude >= 295 & Latitude < 0 & Latitude > -45) | (Longitude <= 30 & Latitude < 0 & Latitude > -45) ~ "SATL", #changed between 2&3
    (Longitude >= 265 & Latitude > 25 & Latitude < 55) | (Longitude < 100 & Latitude > 25 & Latitude < 55) ~ "NATL", #changed between 2&3
    Longitude <= 100 & Longitude >= 30 & Latitude < 30 & Latitude > -15 ~ "NIND",
    Longitude < 150 & Longitude > 100 & Latitude <= 60 & Latitude > 0 ~ "SEAS", #changed between v2 and v3
    Longitude <= 265 & Longitude > 150 & Latitude < 55 & Latitude >= 25 ~ "NPAC", #changed between v2 and v3
    Longitude >= 0 & Latitude >= 55 & Latitude <= 90 ~ "ARCT", #changed between v2 and v3
    Longitude < 295 & Longitude > 135 & Latitude <= 0 & Latitude >= -45 ~ "AUSP", #changed between v2 and v3
    Longitude > 30 & Longitude < 135 & Latitude < -15 & Latitude > -45 ~ "SIND", #changed between v2 and v3
    Longitude >= 0 & Latitude <= -45 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > 0 & Latitude < 25) | (Longitude < 30 & Latitude > 0 & Latitude < 25) ~ "CPAO")) #changed between v2 & v3

na_count <- sum(is.na(coal.ash.sims$region_3))
print(na_count)
region_counts <- table(coal.ash.sims$region_3)
print(region_counts)

coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_4= case_when(
    (Longitude >= 295 & Latitude < 0 & Latitude > -45) | (Longitude <= 30 & Latitude < 0 & Latitude > -45) ~ "SATL",  
    (Longitude >= 265 & Latitude > 25 & Latitude < 55) | (Longitude < 100 & Latitude > 25 & Latitude < 55) ~ "NATL", 
    Longitude <= 100 & Longitude > 78 & Latitude < 30 & Latitude > -15 ~ "Bay of Bengal",
    Longitude <= 78 & Longitude >= 30 & Latitude < 30 & Latitude > -15 ~ "Arabian Sea",
    Longitude < 150 & Longitude > 100 & Latitude <= 60 & Latitude > 0 ~ "SEAS", 
    Longitude <= 265 & Longitude > 150 & Latitude < 55 & Latitude >= 25 ~ "NPAC", 
    Longitude >= 0 & Latitude >= 55 & Latitude <= 90 ~ "ARCT", 
    Longitude < 295 & Longitude > 135 & Latitude <= 0 & Latitude >= -45 ~ "AUSP", 
    Longitude > 30 & Longitude < 135 & Latitude < -15 & Latitude > -45 ~ "SIND", 
    Longitude >= 0 & Latitude <= -45 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > 0 & Latitude < 25) | (Longitude < 30 & Latitude > 0 & Latitude < 25) ~ "CPAO")) 

na_count <- sum(is.na(coal.ash.sims$region_4))
print(na_count)
region_counts <- table(coal.ash.sims$region_4)
print(region_counts)

# tightest areas examining specific increases to soluble fe signal 
coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_resicoal = case_when(
    Longitude <= 100 & Longitude > 80 & Latitude < 20 & Latitude > 0 ~ "Bay of Bengal",
    Longitude <= 75 & Longitude >= 50 & Latitude < 25 & Latitude > 0 ~ "Arabian Sea",
    Longitude < 150 & Longitude > 105 & Latitude <= 47 & Latitude > 7 ~ "Southeastern Asia", 
    Longitude <= 40 & Longitude > 5 & Latitude < 60 & Latitude >= 30 ~ "Mediterranean Sea", 
    Longitude <= 42 & Longitude > 8 & Latitude >= -20 & Latitude <= -40 ~ "Cape of Africa", # no observations here apparently
    (Longitude >= 300 & Latitude > -40 & Latitude < -5) | (Longitude < 330 & Latitude > -40 & Latitude < -5) ~ "Coastal Brazil", 
    (Longitude >= 270 & Latitude > 25 & Latitude < 50) | (Longitude < 25 & Latitude > 25 & Latitude < 50) ~ "East Coast USA"))

na_count <- sum(is.na(coal.ash.sims$region_resicoal))
print(na_count)
region_counts <- table(coal.ash.sims$region_resicoal)
print(region_counts)

coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_indcoal = case_when(
    Longitude <= 100 & Longitude > 80 & Latitude < 20 & Latitude > 0 ~ "Bay of Bengal",
    Longitude <= 75 & Longitude >= 50 & Latitude < 25 & Latitude > 0 ~ "Arabian Sea",
    Longitude < 135 & Longitude > 110 & Latitude <= 25 & Latitude > -5 ~ "Southeastern Asia", 
    (Longitude >= 305 & Latitude > -85 & Latitude < -60) | (Longitude < 20 & Latitude > -85 & Latitude < -60) ~ "Antarctica", 
    Longitude <= 40 & Longitude > 5 & Latitude < 70 & Latitude >= 50 ~ "Scandinavia", 
    (Longitude >= 355 & Latitude > -5 & Latitude < 15) | (Longitude < 15& Latitude > -5 & Latitude < 15)~ "Central Africa", 
    (Longitude >= 300 & Latitude > -40 & Latitude < -5) | (Longitude < 330 & Latitude > -40 & Latitude < -5) ~ "Coastal Brazil", 
    (Longitude >= 270 & Latitude > 25 & Latitude < 50) | (Longitude < 300 & Latitude > 25 & Latitude < 50) ~ "East Coast USA",
    (Longitude >= 220 & Latitude > 30 & Latitude < 60) | (Longitude < 250 &  Latitude > 30 & Latitude < 60) ~ "West Coast USA"))

na_count <- sum(is.na(coal.ash.sims$region_indcoal))
print(na_count)
region_counts <- table(coal.ash.sims$region_indcoal)
print(region_counts)


# -------------------------------------- VISUALIZATIONS --------------------------------------------
# Linear Regression plots comparing obs and modeled values independent of region on a log scale ------------------------------------------
# at the highest resolution
lm_model.n1 <- lm(v1_Fe ~ Obs_Fe, data = coal.ash.sims)
# Extract R-squared value from the linear model
r_squared <- summary(lm_model.n1)$r.squared

#issues with introducing the error bars because they aren't as easily plotted on a logscale, so I might need to calculate the log-transformed values in the df and then plot 
# calculating log values 

Total.Fe.n1 <-ggplot(coal.ash.sims, aes(x = Obs_Fe, y = v1_Fe)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Total [Fe] (ng m-3)", y = "Modeled Total [Fe] (ng m-3)", title = "Observed vs. MIMI Iron Concentrations Present Day") +
  #geom_errorbar(aes(ymin = Obs_Fe - Obs_Fe_sd, ymax = Obs_Fe + Obs_Fe_sd, width = 0.1, size = 1) +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "left", label.y = "top", size = 4) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 100000), xlim = c(-1, 100000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) 

Soluble.Fe.n1 <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = v1_sol_Fe)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] (ng m-3)", title = "Observed vs. MIMI Soluble Iron Concentrations Present Day")  +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "left", label.y = "top", size = 4) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) 

Total.Fe.PI <- ggplot(coal.ash.sims, aes(x = Obs_Fe, y = PI.v1_Fe)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Total [Fe] (ng m-3)", y = "Modeled Total [Fe] (ng m-3)", title = "Observed vs. MIMI Iron Concentrations Pre-Industrial") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "left", label.y = "top", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  theme_minimal() + 
  scale_x_log10() + 
  scale_y_log10() + 
  coord_cartesian(ylim = c(-1, 100000), xlim = c(-1, 100000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", linewidth = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", linewidth = 1) 

Soluble.Fe.PI <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = PI.v1_sol_Fe)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] (ng m-3)", title = "Observed vs. MIMI Soluble Iron Concentrations Pre-Industrial")  +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "left", label.y = "top", size = 4) +
  theme_minimal() + 
  scale_x_log10() + 
  scale_y_log10() + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) 

Total.Fe.n1 + Soluble.Fe.n1 + Total.Fe.PI + Soluble.Fe.PI 
  
# Comparing the simulations plots color palette  -------------------- 
# set color palette
library(RColorBrewer);library(ggbreak) 
unique(coal.ash.sims$region)
brewer.pal(n = 8, name = "Dark2")
coal.ash.palette <- c("ARCT" = "#A6CEE3",
                     "SO"   = "#1F78B4",
                     "NATL" = "#B2DF8A",
                     "SATL" = "#33A02C",
                     "CPAO" = "#FB9A99", 
                     "NPAC" = "#E31A1C", 
                     "SEAS" = "#FDBF6F", 
                     "AUSP" = "#FF7F00", 
                     "NIND" = "#CAB2D6", 
                     "SIND" = "#6A3D9A")

coal.ash.palette.2 <- c("ARCT" = "#1B9E7780",
                      "SO"   = "#1F78B480" ,
                      "NATL" = "#7570B380",
                      "SATL" = "#E7298A80",
                      "CPAO" = "#66A61E80" , 
                      "NPAC" = "#E6AB0280" , 
                      "SEAS" = "#A6761D80", 
                      "AUSP" = "#66666680", 
                      "NIND" = "#D95F0280", 
                      "SIND" = "#FF7F0080")

India.palette <- c("ARCT" = "#1B9E7780",
                        "SO"   = "#1F78B480" ,
                        "NATL" = "#7570B380",
                        "SATL" = "#E7298A80",
                        "CPAO" = "#66A61E80" , 
                        "NPAC" = "#E6AB0280" , 
                        "SEAS" = "#A6761D80", 
                        "AUSP" = "#66666680", 
                        "Bay of Bengal" = "#D95F0280", 
                        "Arabian Sea" = "#2d519280", 
                        "SIND" = "#FF7F0080")

ResiCoal.palette <- c("Cape of Africa" = "#c5489980",
                      "Mediterranean Sea"  = "#3c885b80" ,
                   "East Coast USA" = "#8460ca80" , 
                   "Coastal Brazil" = "#E6AB0280" , 
                   "Southeastern Asia" = "#A6761D80", 
                   "Bay of Bengal" = "#D95F0280", 
                   "Arabian Sea" = "#2d519280")

IndCoal.palette <- c("Central Africa" = "#8c3c3480",
                      "Mediterranean Sea"  = "#3c885b80" ,
                      "East Coast USA" = "#8460ca80" ,
                      "West Coast USA" = "#dd545380",
                      "Antarctica" = "#1B9E7780",
                      "Coastal Brazil" = "#E6AB0280" , 
                      "Southeastern Asia" = "#A6761D80", 
                      "Scandinavia" = "#5cabda80",
                      "Bay of Bengal" = "#D95F0280", 
                      "Arabian Sea" = "#2d519280")

# Regression plots -------------------------------------------------------
coal.ash.sims.sol <- coal.ash.sims %>% drop_na(Obs_sol_Fe)

create_soluble_iron_plot <- function(data, x_var, y_var, region, color_palette, 
                                     x_label = "Observed Soluble [Fe] (ng m-3)", 
                                     y_label = "Modeled Soluble [Fe] (ng m-3)", 
                                     plot_title = "Observed vs. Modeled Soluble Iron") {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(region))) +
    geom_point() +  # Add points
    geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
    labs(x = x_label, y = y_label, title = plot_title) +
    stat_poly_eq(formula = y ~ x, 
                 eq.with.lhs = "italic(y)~`=`~",
                 aes(label = paste(..eq.label.., sep = "*\", \"*")), 
                 parse = TRUE,
                 label.x = "right", label.y = "bottom", size = 4) +
    scale_color_manual(values = color_palette) +
    theme_minimal() + 
    scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
    scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
    coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
    theme(plot.title = element_text(size = 14, face = "bold")) + 
    theme(legend.position="none")
}

create_total_iron_plot <- function(data, x_var, y_var, region, color_palette, 
                                     x_label = "Observed Total [Fe] (ng m-3)", 
                                     y_label = "Modeled Total [Fe] (ng m-3)", 
                                     plot_title = "Observed vs. Modeled Total Iron") {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(region))) +
    geom_point() +  # Add points
    geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
    labs(x = x_label, y = y_label, title = plot_title) +
    stat_poly_eq(formula = y ~ x, 
                 eq.with.lhs = "italic(y)~`=`~",
                 aes(label = paste(..eq.label.., sep = "*\", \"*")), 
                 parse = TRUE,
                 label.x = "right", label.y = "bottom", size = 4) +
    scale_color_manual(values = color_palette) +
    theme_minimal() + 
    scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
    scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
    coord_cartesian(ylim = c(-1, 100000), xlim = c(-1, 100000)) + 
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
    theme(plot.title = element_text(size = 14, face = "bold")) + 
    theme(legend.position="none")
}

create_percent_iron_plot <- function(data, x_var, y_var, region, color_palette, 
                                   x_label = "Observed Fe Solubility (%)", 
                                   y_label = "Modeled Fe Solubility (%)", 
                                   plot_title = "Observed vs. Modeled Iron Solubility") {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(region))) +
    geom_point() +  # Add points
    geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
    labs(x = x_label, y = y_label, title = plot_title) +
    stat_poly_eq(formula = y ~ x, 
                 eq.with.lhs = "italic(y)~`=`~",
                 aes(label = paste(..eq.label.., sep = "*\", \"*")), 
                 parse = TRUE,
                 label.x = "right", label.y = "bottom", size = 4) +
    scale_color_manual(values = color_palette) +
    theme_minimal() + 
    coord_cartesian(ylim = c(0, .25), xlim = c(0, .25)) + 
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
    geom_abline(slope = 1, intercept = 0.1, color = "black", linetype = "dashed", size = 1) +
    geom_abline(slope = 1, intercept = -0.1, color = "black", linetype = "dashed", size = 1) +
    theme(plot.title = element_text(size = 14, face = "bold")) + 
    theme(legend.position="none")
}

# create_soluble_iron_plot(df, "x_var", "y_var", "regional grouping", color.palette)
PD.v1.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v1_sol_Fe", "region_4", India.palette)
PD.v1.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v1_Fe", "region_4", India.palette)
PD.v1.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v1_solubility", "region_4", India.palette)

PD.v2.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v2_sol_Fe", "region_4", India.palette)
PD.v2.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v2_Fe", "region_4", India.palette)
PD.v2.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v2_solubility", "region_4", India.palette)

PD.v3.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v3_sol_Fe", "region_4", India.palette)
PD.v3.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v3_Fe", "region_4", India.palette)
PD.v3.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v3_solubility", "region_4", India.palette)

PD.v4.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v4_sol_Fe", "region_4", India.palette)
PD.v4.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v4_Fe", "region_4", India.palette)
PD.v4.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v4_solubility", "region_4", India.palette)

PD.v5.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v5_sol_Fe", "region_4", India.palette)
PD.v5.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v5_Fe", "region_4", India.palette)
PD.v5.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v5_solubility", "region_4", India.palette)

PI.v1.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "PI.v1_sol_Fe", "region_4", India.palette)
PI.v1.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "PI.v1_Fe", "region_4", India.palette)
PI.v1.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "PI.v1_solubility", "region_4", India.palette)

PI.v2.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "PI.v2_sol_Fe", "region_4", India.palette)
PI.v2.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "PI.v2_Fe", "region_4", India.palette)
PI.v2.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "PI.v2_solubility", "region_4", India.palette)

PI.v3.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "PI.v3_sol_Fe", "region_4", India.palette)
PI.v3.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "PI.v3_Fe", "region_4", India.palette)
PI.v1.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "PI.v3_solubility", "region_4", India.palette)

# each PD plot to compare specific changes to solubility 

# comparing PD to PI 
## version 1 (base MIMI)
PI.v1.reg.plot.sol + PD.v1.reg.plot.sol + PI.v1.reg.plot.tot + PD.v1.reg.plot.tot

## version 2 (ask Douglas during meeting)

## version 3 (ask Douglas during meeting)

PD.v1.reg.plot.sol + PD.v2.reg.plot.sol + PD.v3.reg.plot.sol + PD.v4.reg.plot.sol + PD.v5.reg.plot.sol 

PI.v1.reg.plot.sol+ PI.v2.reg.plot.sol + PI.v3.reg.plot.sol

# making single points visualized in regression for easier interpretation ------------------------------
region_count <- coal.ash.sims %>% group_by(region_4) %>% summarize(count = n())

coal.ash.sims.1 <- coal.ash.sims %>% 
                   dplyr::select(-Latitude, -Longitude, -region_1, -region_2, -region_3, -region_resicoal, -region_indcoal) %>%
                   group_by(region_4) %>%
                   summarize(across(everything(), median, na.rm = TRUE)) %>% 
                   ungroup() %>% 
                   full_join(region_count) %>% 
                   mutate(count = as.numeric(count))

create_soluble_iron_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, 
                                     x_label = "Observed Soluble [Fe] (ng m-3)", 
                                     y_label = "Modeled Soluble [Fe] v1 (ng m-3)", 
                                     plot_title = "Observed vs. Modeled Soluble Iron") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = region, size = count_col)) +
    geom_point() +  # Add points
    scale_color_manual(values = color_palette) +
    scale_size(range = c(3, 20)) +
    theme_minimal() + 
    scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
    scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
    coord_cartesian(ylim = c(-0.1, 1000), xlim = c(-0.1, 1000)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    theme(legend.position = "none")
}

create_total_iron_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, 
                                                   x_label = "Observed [Fe] (ng m-3)", 
                                                   y_label = "Modeled [Fe] v1 (ng m-3)", 
                                                   plot_title = "Observed vs. Modeled Total Iron") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = region, size = count_col)) +
    geom_point() +  # Add points
    scale_color_manual(values = color_palette) +
    scale_size(range = c(3, 20)) +
    theme_minimal() + 
    scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
    scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
    coord_cartesian(ylim = c(-0.1, 1000), xlim = c(-0.1, 1000)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold")) + 
    theme(legend.position = "none")
}

# list out the following (df, "x_var", "y_var", region, color_palette, "count")
PD.v1.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v1_sol_Fe", "region_4", India.palette , "count")
PD.v1.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v1_Fe", "region_4", India.palette, "count")

PD.v2.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v2_sol_Fe", "region_4", India.palette, "count")
PD.v2.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v2_Fe", "region_4", India.palette, "count")

PD.v3.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v3_sol_Fe", "region_4", India.palette, "count")
PD.v3.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v3_Fe", "region_4", India.palette, "count")

PD.v4.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v4_sol_Fe", "region_4", India.palette, "count")
PD.v4.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v4_Fe", "region_4", India.palette, "count")

PD.v5.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v5_sol_Fe", "region_4", India.palette, "count")
PD.v5.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v5_Fe", "region_4", India.palette, "count")

PI.v1.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "PI.v1_sol_Fe", "region_4", India.palette, "count")
PI.v1.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "PI.v1_Fe", "region_4", India.palette, "count")

PI.v2.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "PI.v2_sol_Fe", "region_4", India.palette, "count")
PI.v2.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "PI.v2_Fe", "region_4", India.palette, "count")

PI.v3.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "PI.v3_sol_Fe", "region_4", India.palette, "count")
PI.v3.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "PI.v3_Fe", "region_4", India.palette, "count")

PD.v1.dot.plot.sol + PD.v2.dot.plot.sol + PD.v3.dot.plot.sol + PD.v4.dot.plot.sol + PD.v5.dot.plot.sol 

PI.v1.dot.plot.sol+ PI.v2.dot.plot.sol + PI.v3.dot.plot.sol









# parsing out NIND ---------------------------------------------------------------------
india.data <- coal.ash.sims %>% dplyr::filter(region_4 == c("Arabian Sea", "Bay of Bengal"))

v1.Fe.soluble.r4 <- ggplot(india.data, aes(x = Obs_sol_Fe, y = v1_sol_Fe, color = region_4)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v1 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = India.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

v2.Fe.soluble.r4 <- ggplot(india.data, aes(x = Obs_sol_Fe, y = v2_sol_Fe, color = region_4)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v2 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = India.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

v3.Fe.soluble.r4 <- ggplot(india.data, aes(x = Obs_sol_Fe, y = v3_sol_Fe, color = region_4)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v3 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = India.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

v4.Fe.soluble.r4 <- ggplot(india.data, aes(x = Obs_sol_Fe, y = v4_sol_Fe, color = region_4)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v4 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = India.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

v1.Fe.soluble.r4 + v2.Fe.soluble.r4 + v3.Fe.soluble.r4 + v4.Fe.soluble.r4

#Residential coal changes
resicoal.mimi <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = v1_sol_Fe, color = region_resicoal)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v4 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = ResiCoal.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

resicoal.v2 <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = v2_sol_Fe, color = region_resicoal)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v4 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = ResiCoal.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

resicoal.mimi + resicoal.v2

indcoal.mimi + indcoal.v4
indcoal.mimi <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = v1_sol_Fe, color = region_indcoal)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v4 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = IndCoal.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

indcoal.v4 <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = v4_sol_Fe, color = region_indcoal)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Observed Soluble [Fe] (ng m-3)", y = "Modeled Soluble [Fe] v4 (ng m-3)", title = "Observed vs. Modeled Iron Concentrations") +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  # annotate("text", x = min(obs.mod.comparison$Obs_Fe), y = max(obs.mod.comparison$Mod_Fe), 
  # label = paste("R^2 =", round(r_squared, 3)), vjust = 0, hjust = 0) + this R^2 still isn't working
  scale_color_manual(values = IndCoal.palette) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 1000), xlim = c(-1, 1000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold"))

indcoal.mimi + indcoal.v4

# SAVING THE REGRESSION EQUATIONS --------------------------
# getting differences between stat_poly_eq and lm_model... WHY bc of log10 transformations duh
library(broom)
calculate_regression_coefficients_v1_1 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_1 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v1_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
return(coefficients_df)
}

calculate_regression_coefficients_v2_1 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_1 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v2_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v3_1 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_1 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v3_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v4_1 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_1 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v4_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v1_2 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_2 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v1_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v2_2 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_2 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v2_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v3_2 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_2 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v3_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v4_2 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_2 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v4_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v1_3 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_3 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v1_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v2_3 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_3 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v2_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v3_3 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_3 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v3_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

calculate_regression_coefficients_v4_3 <- function(region_name) {
  # Subset the data for regional groupings
  subset_data <- coal.ash.sims.sol %>% filter(region_3 == region_name) 
  # Fit linear regression model and specify model simulation
  lm_model <- lm(log10(v4_sol_Fe) ~ log10(Obs_sol_Fe), data = subset_data)
  coefficients_df <- tidy(lm_model)
  coefficients_df$region <- region_name
  return(coefficients_df)
}

region_names <- c("ARCT", "AUSP", "CPAO", "NATL", "NIND", "NPAC", "SATL", "SEAS", "SIND", "SO")  # Example region names

coefficients_list_v1_1 <- lapply(region_names, calculate_regression_coefficients_v1_1)
coefficients_list_v2_1 <- lapply(region_names, calculate_regression_coefficients_v2_1)
coefficients_list_v3_1 <- lapply(region_names, calculate_regression_coefficients_v3_1)
coefficients_list_v4_1 <- lapply(region_names, calculate_regression_coefficients_v4_1)
coefficients_list_v1_2 <- lapply(region_names, calculate_regression_coefficients_v1_2)
coefficients_list_v2_2 <- lapply(region_names, calculate_regression_coefficients_v2_2)
coefficients_list_v3_2 <- lapply(region_names, calculate_regression_coefficients_v3_2)
coefficients_list_v4_2 <- lapply(region_names, calculate_regression_coefficients_v4_2)
coefficients_list_v1_3 <- lapply(region_names, calculate_regression_coefficients_v1_3)
coefficients_list_v2_3 <- lapply(region_names, calculate_regression_coefficients_v2_3)
coefficients_list_v3_3 <- lapply(region_names, calculate_regression_coefficients_v3_3)
coefficients_list_v4_3 <- lapply(region_names, calculate_regression_coefficients_v4_3)

# Combine coefficients from all regions into a single dataframe
all_coefficients_v1_1 <- do.call(rbind, coefficients_list_v1_1) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v2_1 <- do.call(rbind, coefficients_list_v2_1) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v3_1 <- do.call(rbind, coefficients_list_v3_1) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v4_1 <- do.call(rbind, coefficients_list_v4_1) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v1_2 <- do.call(rbind, coefficients_list_v1_2) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v2_2 <- do.call(rbind, coefficients_list_v2_2) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v3_2 <- do.call(rbind, coefficients_list_v3_2) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v4_2 <- do.call(rbind, coefficients_list_v4_2) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v1_3 <- do.call(rbind, coefficients_list_v1_3) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v2_3 <- do.call(rbind, coefficients_list_v2_3) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v3_3 <- do.call(rbind, coefficients_list_v3_3) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)
all_coefficients_v4_3 <- do.call(rbind, coefficients_list_v4_3) %>% dplyr::select(term, estimate, region) %>% pivot_wider(names_from = term, values_from = estimate)

# Define simulation and regional values
simulation_values <- 1:4
regional_values <- 1:3

# Define a function to extract and combine coefficients for a given simulation and region
extract_and_combine <- function(simulation, region) {
  df_name <- paste0("all_coefficients_v", simulation, "_", region)
  df <- get(df_name)
  df$Simulation <- simulation
  df$Region <- region
  return(df)
}

# Generate combinations of simulation and region values
combinations <- expand.grid(Simulation = simulation_values, Region = regional_values)

# Apply the function to all combinations of simulation and region
coefficients_list <- Map(extract_and_combine, combinations$Simulation, combinations$Region)

library(openxlsx)

# Define the filename for the Excel file
excel_filename <- "C:\\Users\\heplaas\\Desktop\\coefficients_data.xlsx"

# Write each dataframe in the list to a separate sheet in the Excel file
write.xlsx(coefficients_list, excel_filename)



# boxplots to show differences between modeled and simulated Fe solubilities, grouped by region --------------
# Soluble Iron concentrations
coal.ash.sims.soluble <- coal.ash.sims %>% 
  dplyr::select(region_1, region_2, region_3, ends_with("sol_Fe")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("sol_Fe"), names_to = c("data_source", ".value"), names_sep = "_") %>%
  rename(Soluble_Fe = sol)

library(scales)
library(ggpubr)
fspec = function(x) ifelse(x<25, x, 25+(x-25)/10)
fspec_1 = function(x) ifelse(x<25, x, 25+(x-25)*10)
specTrans = trans_new(name = "specialTras",
                      transform = fspec,
                      inverse = fspec_1,
                      breaks = c(0, 25, 100, 200))

ggplot(coal.ash.sims.soluble, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
  facet_wrap(~ region_1) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Regional Groupings v1") +
  theme_bw() +
 coord_trans(y = specTrans) + 
 # geom_jitter(width = 0.2) +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) +
 stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.5)

ggplot(coal.ash.sims.soluble, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) +
  facet_wrap(~ region_1) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Average Soluble Iron Concentration by Region") +
  theme_bw() +
  coord_trans(y = specTrans) + 
  coord_cartesian(ylim = c(0, 7.5)) + theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 18,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 20, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) 

ggplot(coal.ash.sims.soluble, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) +
  facet_wrap(~ region_2) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Regional Groupings v2") +
  theme_bw() +
  coord_trans(y = specTrans) + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.5)

ggplot(coal.ash.sims.soluble, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) +
  facet_wrap(~ region_3) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Regional Groupings v3") +
  theme_bw() +
  coord_trans(y = specTrans) + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.5)


# Solubility in % ----------------------------------------------
coal.ash.sims.solubility.x <- coal.ash.sims %>% 
  dplyr::select(region_1, region_2, region_3, ends_with("solubility")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("solubility"), names_to = c("data_source", ".value"), names_sep = "_") 

ggplot(coal.ash.sims.solubility.x, aes(x = data_source, y = solubility, fill = data_source)) +
  geom_boxplot() +
  facet_wrap(~ region_1) +
  labs(x = "Data Source", y = "Fraction Soluble Iron", title = "Iron Solubility by Regional Grouping 1") +
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) + 
  coord_cartesian(ylim = c(0, .15)) +
 stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.005, vjust = 0.05, position = "jitter", label.y = 0)

ggplot(coal.ash.sims.solubility.x, aes(x = data_source, y = solubility, fill = data_source)) +
  geom_boxplot() +
  facet_wrap(~ region_2) +
  labs(x = "Data Source", y = "Fraction Soluble Iron", title = "Iron Solubility by Regional Grouping 2") +
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) + 
  coord_cartesian(ylim = c(0, .15)) +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.005, vjust = 0.05, position = "jitter", label.y = 0)

ggplot(coal.ash.sims.solubility.x, aes(x = data_source, y = solubility, fill = data_source)) +
  geom_boxplot() +
  facet_wrap(~ region_3) +
  labs(x = "Data Source", y = "Fraction Soluble Iron", title = "Iron Solubility by Regional Grouping 3") +
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) + 
  coord_cartesian(ylim = c(0, .15)) +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.005, vjust = 0.05, position = "jitter", label.y = 0)

# Parsing out India  ----------------------------------------------
india.data.x <- india.data %>% 
  dplyr::select(region_4, ends_with("solubility")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("solubility"), names_to = c("data_source", ".value"), names_sep = "_") 

ggplot(india.data.x, aes(x = data_source, y = solubility, fill = data_source)) +
  geom_boxplot() +
  facet_wrap(~ region_4) +
  labs(x = "Data Source", y = "Fraction Soluble Iron", title = "Iron Solubility by Regional Grouping 4") +
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) + 
  coord_cartesian(ylim = c(0, .15)) +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.1, vjust = 0.01, position = "jitter", label.y = 0.1)






# Plots to compare PD vs PI simulations 
# v1 (base MIMI)
MIMI_PI_PD <- ggplot(coal.ash.sims.sol, aes(x = v1_Fe, y = PI.v1_Fe, color = region_1)) +
  geom_point()  +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Present Day [Fe] (ng m-3)", y = "Pre-Industrial [Fe] v1 (ng m-3)", title = "Base MIMI simulation") +
  stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  scale_color_manual(values = coal.ash.palette.2) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 100000), xlim = c(-1, 100000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(legend.position="none") 

v2_PI_PD <- ggplot(coal.ash.sims.sol, aes(x = v3_Fe, y = PI.v2_Fe, color = region_1)) +
  geom_point()  +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Present Day [Fe] (ng m-3)", y = "Pre-Industrial [Fe] v1 (ng m-3)", title = "Changes to Biofuel") +
  stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  scale_color_manual(values = coal.ash.palette.2) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 100000), xlim = c(-1, 100000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(legend.position="none") 

v3_PI_PD <- ggplot(coal.ash.sims.sol, aes(x = v2_Fe, y = PI.v3_Fe, color = region_1)) +
  geom_point()  +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Present Day [Fe] (ng m-3)", y = "Pre-Industrial [Fe] v1 (ng m-3)", title = "Changes to Fire/Wood") +
  stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  scale_color_manual(values = coal.ash.palette.2) +
  theme_minimal() + 
  scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
  scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
  coord_cartesian(ylim = c(-1, 100000), xlim = c(-1, 100000)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(legend.position="none") 

MIMI_PI_PD + v2_PI_PD + v3_PI_PD 

MIMI_PI_PD.solubility <- ggplot(coal.ash.sims.sol, aes(x = v1_solubility, y = PI.v1_solubility, color = region_1)) +
  geom_point()  +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Present Day Fe Solubility (%)", y = "Pre-Industrial Fe Solubility (%)", title = "Base MIMI simulation") +
  stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  scale_color_manual(values = coal.ash.palette.2) +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, .15), xlim = c(0,.15)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(legend.position="none") 

v2_PI_PD.solubility <- ggplot(coal.ash.sims.sol, aes(x = v3_solubility, y = PI.v2_solubility, color = region_1)) +
  geom_point()  +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Present Day Fe Solubility (%)", y = "Pre-Industrial Fe Solubility (%)", title = "Changes to Biofuel") +
  stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  scale_color_manual(values = coal.ash.palette.2) +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, .15), xlim = c(0,.15)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(legend.position="none") 

v3_PI_PD.solubility <- ggplot(coal.ash.sims.sol, aes(x = v2_solubility, y = PI.v3_solubility, color = region_1)) +
  geom_point()  +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without confidence interval
  labs(x = "Present Day Fe Solubility (%)", y = "Pre-Industrial Fe Solubility (%)", title = "Changes to Fire/Wood") +
  stat_poly_eq(formula = y ~ x,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "*\", \"*")), 
               parse = TRUE,
               label.x = "right", label.y = "bottom", size = 4) +
  scale_color_manual(values = coal.ash.palette.2) +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, .15), xlim = c(0,.15)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5) + 
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(legend.position="none") 

MIMI_PI_PD.solubility + v2_PI_PD.solubility + v3_PI_PD.solubility

coal.ash.sims.PI.PD.sol <- coal.ash.sims %>% 
  dplyr::select(region_1, region_2, region_3, ends_with("sol_Fe")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("sol_Fe"), names_to = c("data_source", ".value"), names_sep = "_") %>%
  rename(Soluble_Fe = sol) %>% 
  filter(data_source != "Obs", data_source != "v4") 

library(scales)
library(ggpubr)
fspec = function(x) ifelse(x<25, x, 25+(x-25)/10)
fspec_1 = function(x) ifelse(x<25, x, 25+(x-25)*10)
specTrans.2 = trans_new(name = "specialTras",
                      transform = fspec,
                      inverse = fspec_1,
                      breaks = c(0, 5, 10, 50, 100))

ggplot(coal.ash.sims.PI.PD.sol, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) +
  facet_wrap(~ region_2) +
  labs(x = "Simulation", y = "Soluble Iron (ng m-3)", title = "") +
  theme_bw() +
  ylim(0, 25) +
 # coord_trans(y = specTrans) + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) +
  stat_compare_means(comparisons = list(c("v1", "PI.v1"), c("v3", "PI.v2"), c("v2", "PI.v3")), 
                     label = "p.signif", 
                     method = "wilcox",
                     hide.ns = T,
                     tip.length = 0,
                     textsize = 3,
                     bracket.size = 1,
                     step.increase = 0.02,
                     vjust = 0.1, 
                     label.y = 10) 
 









