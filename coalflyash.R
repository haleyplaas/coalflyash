setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())
#.rs.restartR() # turn on when need to clear up RAM space

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(patchwork);library(readxl);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

#install.packages("")
# ------------------------------ READING IN AND CLEANING DATA -------------------------------------
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

# this version is for when multiple timepoints are present (the x1 runs in the coalflyash case)
 generate_median_values_from_netcdf_PI <- function(file_directory, var1_name, var2_name) {
  # Open the NetCDF file
  nc_data <- nc_open(file_directory)
  
  # Extract latitude and longitude options
  all_long_options <- ncvar_get(nc_data, "lon")
  all_lat_options <- ncvar_get(nc_data, "lat")
  
  # Extract the specified variables
  variable1 <- ncvar_get(nc_data, var1_name)
  variable2 <- ncvar_get(nc_data, var2_name)
  
  # only run this if you need to see the dimensions to double check them 
  # dim(variable.v3)
  # Integrate this code when filtering specific time or altitudes depending on dimensions
  # XXX <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface

  # Calculate median values for the specified variables
  median_var1 <- apply(variable1, c(1, 2), mean, na.rm = TRUE)
  median_var2 <- apply(variable2, c(1, 2), mean, na.rm = TRUE)
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Convert results to data frames with latitude and longitude columns
  median_var1_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    median_value = as.vector(median_var1))
  
  median_var2_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    median_value = as.vector(median_var2))
  
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
  list(coords_wide_var1, coords_wide_var2)}

# this version is for when the mean has already been calculated online (only one timepoint)
generate_median_values_from_netcdf_PD <- function(file_directory, var1_name, var2_name) {
  # Open the NetCDF file
  nc_data <- nc_open(file_directory)
  
  # Extract latitude and longitude options
  all_long_options <- ncvar_get(nc_data, "lon")
  all_lat_options <- ncvar_get(nc_data, "lat")
  
  # Extract the specified variables
  variable1 <- ncvar_get(nc_data, var1_name)
  variable2 <- ncvar_get(nc_data, var2_name)
  
  # only run this if you need to see the dimensions to double check them 
  # dim(variable.v3)
  # Integrate this code when filtering specific time or altitudes depending on dimensions
  # XXX <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface
  
  # Calculate median values for the specified variables
 # median_var1 <- apply(variable1, c(1, 2), median, na.rm = TRUE)
 # median_var2 <- apply(variable2, c(1, 2), median, na.rm = TRUE)
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Convert results to data frames with latitude and longitude columns
  mean_var1_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    mean_value = as.vector(variable1)
  )
  
  mean_var2_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    mean_value = as.vector(variable2)
  )
  
  # Reshape data frames to wide format
  coords_wide_var1 <- pivot_wider(mean_var1_df, names_from = longitude, values_from = mean_value)
  coords_wide_var2 <- pivot_wider(mean_var2_df, names_from = longitude, values_from = mean_value)
  
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

# v1 (PD) INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FINEFIRE33 -- passed panoply check on 7/5/2024
result <- generate_median_values_from_netcdf_PD("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38_2009-2011MEAN.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v1 <- result[[1]]
coords_wide.sol.med.v1 <- result[[2]]

# v2 (PD) INDCOAL0.2-RESICOAL33-WOOD10-OIL38-FINEFIRE33 <- NOTE NAMING CONVENTION WAS WRONG, WOOD GOT SWITCHED
result <- generate_median_values_from_netcdf_PD("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38_2009-2011MEAN.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v2 <- result[[1]]
coords_wide.sol.med.v2 <- result[[2]]

# v3 (PD) INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FINEFIRE33 <- NOTE NAMING CONVENTION WAS WRONG, WOOD GOT SWITCHED
result <- generate_median_values_from_netcdf_PD("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD10-OIL38_2009-2011MEAN.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v3 <- result[[1]]
coords_wide.sol.med.v3 <- result[[2]]

# v4 (PD) INDCOAL0.05-RESICOAL33-WOOD56-OIL25-FINEFIRE33
result <- generate_median_values_from_netcdf_PD("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25_2009-2011MEAN.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v4 <- result[[1]]
coords_wide.sol.med.v4 <- result[[2]]

# v5 (PD) INDCOAL0.05-RESICOAL0.2-WOOD56-OIL25-FIREFINE56 # file name for fire x1 is mismatched with actual settings per Haley's error in PI runs
#result <- generate_median_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25-FIREFINE56.cam.h2.2009-2011_PD.nc", "FETOTSRF", "FESOLSRF")
#coords_wide.med.v5 <- result[[1]]
#coords_wide.sol.med.v5 <- result[[2]]

# v6 (PI) RESICOAL0.2-FIREFINE33 
result <- generate_median_values_from_netcdf_PI("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2_FINEFIRE33_v2.cam.h2.2009-2011_fire_x2.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v6 <- result[[1]]
coords_wide.sol.med.v6 <- result[[2]]

# v7 (PI) RESICOAL33-FIRE33 
result <- generate_median_values_from_netcdf_PI("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE33_v2.cam.h2.2009-2011_fire_x2.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v7 <- result[[1]]
coords_wide.sol.med.v7 <- result[[2]]

# v8 (PI) RESICOAL33-FIRE56
result <- generate_median_values_from_netcdf_PI("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE56_v2.cam.h2.2009-2011_fire_x2.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v8 <- result[[1]]
coords_wide.sol.med.v8 <- result[[2]]

# get dimensions of netcdf files to store for later, diff for PI and PD now  ----------------------------------------
dim.file <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD10-OIL38_2009-2011MEAN.nc"
nc_data <- nc_open(dim.file)
# Extract latitude and longitude options
all_lon_options <- ncvar_get(nc_data, "lon")
all_lat_options <- ncvar_get(nc_data, "lat")
all_lev_options <- ncvar_get(nc_data, "lev") 
all_time_options <- ncvar_get(nc_data, "time")
nc_close(nc_data)

# time is diff for the PI sims
dim.file.PI <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE56_v2.cam.h2.2009-2011_fire_x2.nc"
nc_data.PI <- nc_open(dim.file.PI)
all_time_options_PI <- ncvar_get(nc_data.PI, "time")
nc_close(nc_data.PI)

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- 180.0/length(all_lat_options)
d.lon <- 360/length(all_lon_options)
d.time.PD <- 365/length(all_time_options)
d.time.PI <- 365/length(all_time_options)

# assigning lat and lon to observational data that corresponds with model lat and lons  -------------------
# at the single grid-box resolution (1 grid box, n=1)
# recall, in the araka lamb "c" gridding approach, indicates a range, NOT a cross-sectional point:(0degE, 89.1degN) = (0-1.25degE and 89.1-90degN)
find_model_latitude.n1 <- function(df, all_lat_options) { # this was double checked against the old function (n=4) and passed the QA/QC
  df <- df %>%
    mutate(Model_Latitude_Values = sapply(Latitude, function(lat) {
      closest_lat <- all_lat_options[which.min(abs(all_lat_options - lat))]
      closest_lat
    }))
  return(df)
}
obs.data.n1 <- find_model_latitude.n1(obs.data, all_lat_options)

find_model_longitude.n1 <- function(df, all_lon_options) { # this was double checked against the old function (n=4) and passed the QA/QC
  df <- df %>%
    mutate(Model_Longitude_Values = sapply(Longitude, function(lon) {
      closest_lon <- all_lon_options[which.min(abs(all_lon_options - lon))]
      closest_lon
    }))
  return(df)
}
obs.data.mod.res.n1 <- find_model_longitude.n1(obs.data.n1, all_lon_options)

# Function to collate observational data which fall in the same grid boxes 
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
obs.data.n4 <- find_model_latitude.n4(obs.data, all_lat_options)

find_model_longitude.n4 <- function(df, all_long_options) {
  df <- df %>%
    mutate(Model_Longitude_Values = sapply(Longitude, function(long) {
      matching_indices <- which(abs(all_long_options - long) <= d.lon)
      if (length(matching_indices) > 0) {
        paste(all_long_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  return(df)
}
obs.data.mod.res.n4 <- find_model_longitude.n4(obs.data.n4, all_lon_options)

# combine observational with modeled data ----------------------------------
library(tibble)
# converting Lat and Lon to character variables
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values) 
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values) 

# TOTAL FE 
coords_long.med.v1 <- coords_wide.med.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_Fe = value)

coords_long.med.v2 <- coords_wide.med.v2 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v2_Fe = value)

coords_long.med.v3 <- coords_wide.med.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v3_Fe = value)

coords_long.med.v4 <- coords_wide.med.v4 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v4_Fe = value)

#coords_long.med.v5 <- coords_wide.med.v5 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v5_Fe = value)

coords_long.med.v6 <- coords_wide.med.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v6_Fe = value)

coords_long.med.v7 <- coords_wide.med.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v7_Fe = value)

coords_long.med.v8 <- coords_wide.med.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v8_Fe = value)

all.TOTFe <- full_join(coords_long.med.v1, coords_long.med.v2) %>% full_join(coords_long.med.v3) %>% full_join(coords_long.med.v4) %>% full_join(coords_long.med.v6) %>% full_join(coords_long.med.v7) %>% full_join(coords_long.med.v8) # %>% full_join(coords_long.med.v5)

# SOLUBLE FE 
coords_long.sol.med.v1 <- coords_wide.sol.med.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_sol_Fe = value)

coords_long.sol.med.v2 <- coords_wide.sol.med.v2 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v2_sol_Fe = value)

coords_long.sol.med.v3 <- coords_wide.sol.med.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v3_sol_Fe = value)

coords_long.sol.med.v4 <- coords_wide.sol.med.v4 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v4_sol_Fe = value)

# coords_long.sol.med.v5 <- coords_wide.sol.med.v5 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v5_sol_Fe = value)

coords_long.sol.med.v6 <- coords_wide.sol.med.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v6_sol_Fe = value)

coords_long.sol.med.v7 <- coords_wide.sol.med.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v7_sol_Fe = value)

coords_long.sol.med.v8 <- coords_wide.sol.med.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v8_sol_Fe = value)

all.SOLFe <- full_join(coords_long.sol.med.v1, coords_long.sol.med.v2) %>% full_join(coords_long.sol.med.v3) %>% full_join(coords_long.sol.med.v4) %>% full_join(coords_long.sol.med.v8) %>% full_join(coords_long.sol.med.v6) %>% full_join(coords_long.sol.med.v7) # %>% full_join(coords_long.sol.med.v5)

# merging all coal fly ash dataframes for easier comparisons  --------------------------------
coal.ash.sims <- left_join(all.TOTFe, all.SOLFe) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, Obs_solubility = `Fe solubility_median`, Obs_Fe_sd = `Fe (ng m–3)_sd`, Obs_sol_Fe_sd = `labile Fe (ng m–3)_sd`) %>% mutate(v1_solubility = v1_sol_Fe / v1_Fe, v2_solubility = v2_sol_Fe / v2_Fe, v3_solubility = v3_sol_Fe / v3_Fe, v4_solubility = v4_sol_Fe / v4_Fe, v6_solubility = v6_sol_Fe / v6_Fe, v7_solubility = v7_sol_Fe / v7_Fe, v8_solubility = v8_sol_Fe / v8_Fe) #v5_solubility = v5_sol_Fe / v5_Fe,

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
  mutate(region_4 = case_when(
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

# ------------------------------------- VISUALIZATIONS --------------------------------------------
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

Total.Fe.PI <- ggplot(coal.ash.sims, aes(x = Obs_Fe, y = v6_Fe)) +
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

Soluble.Fe.PI <- ggplot(coal.ash.sims, aes(x = Obs_sol_Fe, y = v6_sol_Fe)) +
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
  
# Color palettes  -------------------- 
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
    coord_cartesian(ylim = c(0.01, 100), xlim = c(0.01, 100)) + 
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
v1.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v1_sol_Fe", "region_4", India.palette)
v1.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v1_Fe", "region_4", India.palette)
v1.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v1_solubility", "region_4", India.palette)

v2.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v2_sol_Fe", "region_4", India.palette)
v2.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v2_Fe", "region_4", India.palette)
v2.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v2_solubility", "region_4", India.palette)

v3.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v3_sol_Fe", "region_4", India.palette)
v3.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v3_Fe", "region_4", India.palette)
v3.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v3_solubility", "region_4", India.palette)

v4.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v4_sol_Fe", "region_4", India.palette)
v4.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v4_Fe", "region_4", India.palette)
v4.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v4_solubility", "region_4", India.palette)

#v5.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v5_sol_Fe", "region_4", India.palette)
#v5.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v5_Fe", "region_4", India.palette)
#v5.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v5_solubility", "region_4", India.palette)

v6.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v6_sol_Fe", "region_4", India.palette)
v6.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v6_Fe", "region_4", India.palette)
v6.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v6_solubility", "region_4", India.palette)

v7.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v7_sol_Fe", "region_4", India.palette)
v7.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v7_Fe", "region_4", India.palette)
v7.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v7_solubility", "region_4", India.palette)

v8.reg.plot.sol <- create_soluble_iron_plot(coal.ash.sims, "Obs_sol_Fe", "v8_sol_Fe", "region_4", India.palette)
v8.reg.plot.tot <- create_total_iron_plot(coal.ash.sims, "Obs_Fe", "v8_Fe", "region_4", India.palette)
v8.reg.plot.percent <- create_percent_iron_plot(coal.ash.sims, "Obs_solubility", "v8_solubility", "region_4", India.palette)

# each PD plot to compare specific changes to solubility 
v1.reg.plot.sol + v2.reg.plot.sol + v3.reg.plot.sol + v4.reg.plot.sol + v6.reg.plot.sol + v7.reg.plot.sol + v8.reg.plot.sol # + v5.reg.plot.sol

# singling out areas of interest
areas_of_interest <- coal.ash.sims %>% filter(region_4 == c("Arabian Sea", "Bay of Bengal","SEAS", "NPAC"))
v1.reg.plot.sol.areas <- create_soluble_iron_plot(areas_of_interest, "Obs_sol_Fe", "v1_sol_Fe", "region_4", India.palette)
v2.reg.plot.sol.areas <- create_soluble_iron_plot(areas_of_interest, "Obs_sol_Fe", "v2_sol_Fe", "region_4", India.palette)
v3.reg.plot.sol.areas <- create_soluble_iron_plot(areas_of_interest, "Obs_sol_Fe", "v3_sol_Fe", "region_4", India.palette)
v4.reg.plot.sol.areas <- create_soluble_iron_plot(areas_of_interest, "Obs_sol_Fe", "v4_sol_Fe", "region_4", India.palette)
#v5.reg.plot.sol.areas <- create_soluble_iron_plot(areas_of_interest, "Obs_sol_Fe", "v5_sol_Fe", "region_4", India.palette)
v1.reg.plot.sol.areas + v2.reg.plot.sol.areas + v3.reg.plot.sol.areas + v4.reg.plot.sol.areas #+ v5.reg.plot.sol.areas

# comparing PD to PI 
## version 1 (base MIMI)
v1.reg.plot.sol + v1.reg.plot.sol + v6.reg.plot.tot + v6.reg.plot.tot

## version 2 V5 vs V7 (Resicoal 0.2 and Fine Fire 56)
# v5.reg.plot.sol + v5.reg.plot.sol + v7.reg.plot.tot + v7.reg.plot.tot

## version 3 V3 vs V8 (Resicoal 33 and Fine Fire 33)
v3.reg.plot.sol + v3.reg.plot.sol + v8.reg.plot.tot + v8.reg.plot.tot

# making single points visualized in regression for easier interpretation ------------------------------
region_count <- coal.ash.sims %>% group_by(region_4) %>% summarize(count = n())

coal.ash.sims.1 <- coal.ash.sims %>% 
                   dplyr::select(-Latitude, -Longitude, -region_1, -region_2, -region_3, -region_resicoal, -region_indcoal) %>%
                   group_by(region_4) %>%
                   summarize(across(everything(), median, na.rm = TRUE)) %>% 
                   ungroup() %>% 
                   full_join(region_count) %>% 
                   mutate(count = as.numeric(count)) %>% drop_na()

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
    scale_x_log10(labels = function(x) format(x, scientific = FALSE)) + 
    scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + 
    coord_cartesian(ylim = c(-0.1, 100), xlim = c(-0.1, 100)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    theme(legend.position = "none")
}

create_total_iron_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, 
                                                   x_label = "Observed [Fe] (ng m-3)", 
                                                   y_label = "Modeled [Fe] (ng m-3)", 
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
    theme(plot.title = element_text(size = 14, face = "bold"))  + 
    theme(legend.position = "none")
}

create_solubility_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, 
                                                   x_label = "Observed Fractional Fe Solubility (%)", 
                                                   y_label = "Modeled Fractional Fe Solubility (%)", 
                                                   plot_title = "Observed vs. Modeled Fe Solubility") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = region, size = count_col)) +
    geom_point() +  # Add points
    scale_color_manual(values = color_palette) +
    scale_size(range = c(3, 20)) +
    theme_minimal() + 
    scale_x_log10(labels = scales::number_format(accuracy = 0.001)) + # change accuracy to 0.001 for % solubility figures
    scale_y_log10(labels = scales::number_format(accuracy = 0.001)) + 
    coord_cartesian(ylim = c(0.01, 0.5), xlim = c(0.01, 0.5)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    theme(legend.position = "none")
}

# list out the following (df, "x_var", "y_var", region, color_palette, "count")
v1.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v1_sol_Fe", "region_4", India.palette , "count")
v1.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v1_Fe", "region_4", India.palette, "count")
v1.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v1_solubility", "region_4", India.palette, "count")

v2.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v2_sol_Fe", "region_4", India.palette, "count")
v2.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v2_Fe", "region_4", India.palette, "count")
v2.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v2_solubility", "region_4", India.palette, "count")

v3.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v3_sol_Fe", "region_4", India.palette, "count")
v3.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v3_Fe", "region_4", India.palette, "count")
v3.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v3_solubility", "region_4", India.palette, "count")

v4.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v4_sol_Fe", "region_4", India.palette, "count")
v4.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v4_Fe", "region_4", India.palette, "count")
v4.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v4_solubility", "region_4", India.palette, "count")

#v5.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v5_sol_Fe", "region_4", India.palette, "count")
#v5.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v5_Fe", "region_4", India.palette, "count")
#v5.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v5_solubility", "region_4", India.palette, "count")

v6.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v6_sol_Fe", "region_4", India.palette, "count")
v6.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v6_Fe", "region_4", India.palette, "count")
v6.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v6_solubility", "region_4", India.palette, "count")

v7.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v7_sol_Fe", "region_4", India.palette, "count")
v7.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v7_Fe", "region_4", India.palette, "count")
v7.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v7_solubility", "region_4", India.palette, "count")

v8.dot.plot.sol <- create_soluble_iron_plot_single_points(coal.ash.sims.1, "Obs_sol_Fe", "v8_sol_Fe", "region_4", India.palette, "count")
v8.dot.plot.tot <- create_total_iron_plot_single_points(coal.ash.sims.1, "Obs_Fe", "v8_Fe", "region_4", India.palette, "count")
v8.dot.plot.per.sol <- create_solubility_plot_single_points(coal.ash.sims.1, "Obs_solubility", "v8_solubility", "region_4", India.palette, "count")

v1.dot.plot.sol + v2.dot.plot.sol + v3.dot.plot.sol + v4.dot.plot.sol + 
  #v5.dot.plot.sol + 
  v6.dot.plot.sol + v7.dot.plot.sol 
#+ v8.dot.plot.sol

v1.dot.plot.sol + v2.dot.plot.sol
#v5.dot.plot.sol + v7.dot.plot.sol
v6.dot.plot.sol + v7.dot.plot.sol + guides(color = guide_legend(override.aes = list(size = 5)))

v1.dot.plot.per.sol + v2.dot.plot.per.sol + v3.dot.plot.per.sol + v4.dot.plot.per.sol + 
  # v5.dot.plot.per.sol + 
  v6.dot.plot.per.sol + v7.dot.plot.per.sol #+ v8.dot.plot.per.sol

v1.dot.plot.tot + v2.dot.plot.tot + v3.dot.plot.tot + v4.dot.plot.tot + v6.dot.plot.tot + v7.dot.plot.tot
v1.dot.plot.tot + v6.dot.plot.tot

# Histogram of observations per each region 
ggplot(coal.ash.sims, aes(x = region_4, fill = region_4, position = "identity")) +
  geom_bar(stat = "count") +
  labs(title = "Histogram of Values", x = "Value", y = "Frequency") +
  theme_minimal() + 
  scale_fill_manual(values = India.palette) + 
  coord_flip() 

# boxplots to show differences between modeled and simulated Fe solubilities, grouped by region --------------
library(ggpubr)

# Soluble Iron concentrations
colnames(coal.ash.sims)
coal.ash.sims.boxplot <- coal.ash.sims %>% 
  dplyr::select(region_4, Obs_sol_Fe, v1_sol_Fe, v2_sol_Fe, v3_sol_Fe, v4_sol_Fe, v6_sol_Fe, v7_sol_Fe, v8_sol_Fe) %>%
  rename(Obs_sol = Obs_sol_Fe, v1_sol = v1_sol_Fe, v2_sol = v2_sol_Fe, v3_sol = v3_sol_Fe, v4_sol = v4_sol_Fe, v6_sol = v6_sol_Fe, v7_sol = v7_sol_Fe, v8_sol = v8_sol_Fe) %>%
  drop_na() %>%
  pivot_longer(cols = ends_with("sol"), names_to = c("data_source", ".value"), names_sep = "_") %>%
  rename(Soluble_Fe = sol) 


# Filter for the groups to compare
coal.ash.sims.boxplot.v1 <- coal.ash.sims.boxplot %>% 
  filter(data_source %in% c("Obs", "v1"))
coal.ash.sims.tbl <- tibble::as_tibble(coal.ash.sims.boxplot.v1)
test_v1 <- coal.ash.sims.tbl %>%
  group_by(region_4) %>%
  do(w = wilcox.test(Soluble_Fe ~ data_source, data = ., paired = FALSE, exact = FALSE)) %>%
  summarise(region_4, Wilcox = w$p.value)
test_v1 <- test_v1 %>%
  mutate(Wilcox = format(Wilcox, scientific = FALSE, digits = 3))
print(test_v1)

coal.ash.sims.boxplot.v2 <- coal.ash.sims.boxplot %>% 
  filter(data_source %in% c("Obs", "v2"))
coal.ash.sims.tbl <- tibble::as_tibble(coal.ash.sims.boxplot.v2)
test_v2 <- coal.ash.sims.tbl %>%
  group_by(region_4) %>%
  do(w = wilcox.test(Soluble_Fe ~ data_source, data = ., paired = FALSE, exact = FALSE)) %>%
  summarise(region_4, Wilcox = w$p.value)
test_v2 <- test_v2 %>%
  mutate(Wilcox = format(Wilcox, scientific = FALSE, digits = 3))
print(test_v2)

coal.ash.sims.boxplot.v3 <- coal.ash.sims.boxplot %>% 
  filter(data_source %in% c("Obs", "v3"))
coal.ash.sims.tbl <- tibble::as_tibble(coal.ash.sims.boxplot.v3)
test_v3 <- coal.ash.sims.tbl %>%
  group_by(region_4) %>%
  filter(data_source %in% c("Obs", "v3")) %>%
  do(w = wilcox.test(Soluble_Fe ~ data_source, data = ., paired = FALSE, exact = FALSE)) %>%
  summarise(region_4, Wilcox = w$p.value)
test_v3 <- test_v3 %>%
  mutate(Wilcox = format(Wilcox, scientific = FALSE, digits = 3))
print(test_v3)

coal.ash.sims.boxplot.v4 <- coal.ash.sims.boxplot %>% 
  filter(data_source %in% c("Obs", "v4"))
coal.ash.sims.tbl <- tibble::as_tibble(coal.ash.sims.boxplot.v4)
test_v4 <- coal.ash.sims.tbl %>%
  group_by(region_4) %>%
  filter(data_source %in% c("Obs", "v4")) %>%
  do(w = wilcox.test(Soluble_Fe ~ data_source, data = ., paired = FALSE, exact = FALSE)) %>%
  summarise(region_4, Wilcox = w$p.value)
test_v4 <- test_v4 %>%
  mutate(Wilcox = format(Wilcox, scientific = FALSE, digits = 3))
print(test_v4)

ggplot(coal.ash.sims.boxplot, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
  facet_wrap(~ region_4) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Soluble Iron Concentrations by Region") +
  theme_bw() +
  scale_fill_brewer(palette = "Spectral") +
  stat_compare_means(comparisons = list(c("Obs", "v1")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.15, vjust = 5, paired = TRUE, exact = FALSE)+
 ylim(0, 20) +
 theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) 

ggplot(coal.ash.sims.boxplot, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
  facet_wrap(~ region_4) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Soluble Iron Concentrations by Region") +
  theme_bw() +
  scale_fill_brewer(palette = "Spectral") +
  ylim(0, 2.5) +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) 

coal.ash.sims.boxplot.2 <- coal.ash.sims %>% 
  dplyr::select(region_4, ends_with("solubility")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("solubility"), names_to = c("data_source", ".value"), names_sep = "_") %>%
  rename(Solubility = solubility) 

#nice view of data
ggplot(coal.ash.sims.boxplot.2, aes(x = data_source, y = Solubility, fill = data_source)) +
  geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
  facet_wrap(~ region_4) +
  labs(x = "Data Source", y = "Fe Solubility (%)", title = "Iron Solubility by Region") +
  theme_bw() +
  scale_fill_brewer(palette = "Spectral") +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.15, vjust = 10) +
  ylim(0, .3) +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) 

# see stats
ggplot(coal.ash.sims.boxplot.2, aes(x = data_source, y = Solubility, fill = data_source)) +
  geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
  facet_wrap(~ region_4) +
  labs(x = "Data Source", y = "Fe Solubility (%)", title = "Iron Solubility by Region") +
  theme_bw() +
  scale_fill_brewer(palette = "Spectral") +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif",   method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.15, vjust = 0) +
  ylim(0, 5) +
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_blank(), legend.text = element_text(size=14), axis.text.x = element_text(size = 18, color="black"), axis.title.x = element_text(size = 20, face = "bold", color="black"),  axis.text.y = element_text(size = 10,  color="black"),  axis.title.y = element_text(size = 20, face = "bold", color="black"), strip.text.x = element_text(size = 15, face = "bold", color="black"), strip.text.y = element_text(size = 20, face = "bold", color="black")) 

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




