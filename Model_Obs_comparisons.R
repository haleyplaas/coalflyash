setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())
#.rs.restartR() # turn on when need to clear up RAM space

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(patchwork);library(readxl);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

#install.packages("")
# ------------------------------ READING IN AND CLEANING DATA -------------------------------------
# Read in observational data (excel file) with specified parameters -----------------------
data.file <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\24.10.16_FeObs_Hamilton2022_updated2024_Tang.xlsx"

obs.data <- read_excel(data.file, sheet =   "Tang_approved_comparisons", #"Hamilton+Plaas", #
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
# Function to generate array of values,condense to single mean value over entire time period
library(ncdf4);library(dplyr);library(tidyr)

# when multiple timepoints still exist -- find averages and standard deviations over time
generate_mean_values_from_netcdf <- function(file_directory, var1_name, var2_name) {
  # Open the NetCDF file
  nc_data <- nc_open(file_directory)
  
  # Extract latitude and longitude options
  all_long_options <- ncvar_get(nc_data, "lon")
  all_lat_options <- ncvar_get(nc_data, "lat")
  
  # Round latitude to 3 decimal places to match the PD files 
  all_lat_options <- round(all_lat_options, 3)
  
  # Extract the specified variables
  variable1 <- ncvar_get(nc_data, var1_name)
  variable2 <- ncvar_get(nc_data, var2_name)
  
  # only run this if you need to see the dimensions to double check them 
  # dim(variable.v3)
  # Integrate this code when filtering specific time or altitudes depending on dimensions
  # XXX <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 56 corresponds to surface
  
  # Calculate mean values for the specified variables
  mean_var1 <- apply(variable1, c(1, 2), mean, na.rm = TRUE)
  mean_var2 <- apply(variable2, c(1, 2), mean, na.rm = TRUE)
  
  # Calculate mean values for the specified variables
  sd_var1 <- apply(variable1, c(1, 2), sd, na.rm = TRUE)
  sd_var2 <- apply(variable2, c(1, 2), sd, na.rm = TRUE)
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Convert results to data frames with latitude and longitude columns
  mean_var1_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    mean_value = as.vector(mean_var1)
  )
  
  mean_var2_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    mean_value = as.vector(mean_var2)
  )
  
  sd_var1_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    sd_value = as.vector(sd_var1)
  )
  
  sd_var2_df <- data.frame(
    latitude = rep(all_lat_options, each = length(all_long_options)),
    longitude = rep(all_long_options, length(all_lat_options)),
    sd_value = as.vector(sd_var2)
  )
  
  # Reshape data frames to wide format
  coords_wide_var1 <- pivot_wider(mean_var1_df, names_from = longitude, values_from = mean_value)
  coords_wide_var2 <- pivot_wider(mean_var2_df, names_from = longitude, values_from = mean_value)
  
  # Reshape sd data frames to wide format
  coords_wide_var1_sd <- pivot_wider(sd_var1_df, names_from = longitude, values_from = sd_value)
  coords_wide_var2_sd <- pivot_wider(sd_var2_df, names_from = longitude, values_from = sd_value)
  
  # Convert tibbles to data frames and set row names
  coords_wide_var1 <- as.data.frame(coords_wide_var1)
  coords_wide_var2 <- as.data.frame(coords_wide_var2)
  rownames(coords_wide_var1) <- all_lat_options
  rownames(coords_wide_var2) <- all_lat_options
  coords_wide_var1 <- coords_wide_var1 %>% dplyr::select(-latitude)
  coords_wide_var2 <- coords_wide_var2 %>% dplyr::select(-latitude)
  
  coords_wide_var1_sd <- as.data.frame(coords_wide_var1_sd)
  coords_wide_var2_sd <- as.data.frame(coords_wide_var2_sd)
  rownames(coords_wide_var1_sd) <- all_lat_options
  rownames(coords_wide_var2_sd) <- all_lat_options
  coords_wide_var1_sd <- coords_wide_var1_sd %>% dplyr::select(-latitude)
  coords_wide_var2_sd <- coords_wide_var2_sd %>% dplyr::select(-latitude)
  
  # Return the wide data frames
  list(coords_wide_var1, coords_wide_var2, coords_wide_var1_sd, coords_wide_var2_sd)
}

# when data is already a mean 
read_in_netcdf <- function(file_directory, var1_name, var2_name) {
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
  # XXX <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 56 corresponds to surface
  
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

# PD SIMULATIONS -----------------------------------------------------
# v1 for testing transient sim
#result <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\CAM6-MIMI-PD-test-20250129_noSoilState.cam.h2.JAN.nc", "FETOTSRF", "FESOLSRF")
# v1 (PD) INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FINEFIRE33
#result <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FETOTSRF", "FESOLSRF")
# NO SOIL STATE VERSIONS
result <- generate_mean_values_from_netcdf("D:\\CoalFlyAsh\\no_soil_state_firex1_runs\\no_soil_state_an+du_cheyenne_bb_derecho.h2_v1.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v1 <- result[[1]]
coords_wide.sol.med.v1 <- result[[2]]
coords_wide.med.v1_sd <- result[[3]]
coords_wide.sol.med.v1_sd <- result[[4]]

# CHEYENNE VERSION -- TOTAL FE v1
#result <- read_in_netcdf("D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38_2009-2011.h2.BBx1_ANx2_DUx2.nc", "FETOTSRF", "FESOLSRF")
#coords_wide.med.v1 <- result[[1]]
#coords_wide.sol.med.v1 <- result[[2]]
# DERECHO VERSION -- TOTAL FE v1
#result <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FETOTSRF", "FESOLSRF")
#coords_wide.med.v1 <- result[[1]]
#coords_wide.sol.med.v1 <- result[[2]]

# v2 (PD) INDCOAL0.2-RESICOAL33-WOOD10-OIL38-FINEFIRE33
# recall that these labels were flipped
#result <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\FHIST_transient_test.cam.h2.2008-JAN.nc", "FETOTSRF", "FESOLSRF")
# Cheyenne version
#result <- read_in_netcdf("D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38_2009-2011.h2.BBx1_ANx2_DUx2.nc", "FETOTSRF", "FESOLSRF")
#coords_wide.med.v2 <- result[[1]]
#coords_wide.sol.med.v2 <- result[[2]]

result <- generate_mean_values_from_netcdf("D:\\CoalFlyAsh\\no_soil_state_firex1_runs\\no_soil_state_an+du_cheyenne_bb_derecho.h2_v2.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v2 <- result[[1]]
coords_wide.sol.med.v2 <- result[[2]]
coords_wide.med.v2_sd <- result[[3]]
coords_wide.sol.med.v2_sd <- result[[4]]

# v3 (PD) INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FINEFIRE33
# recall labels flipped with v2 -- this was true for Cheyenne, not sure if this is true for Derecho version check in on this later 
# Cheyenne Version
#result <- read_in_netcdf("D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD10-OIL38_2009-2011.h2.BBx1_ANx2_DUx2.nc", "FETOTSRF", "FESOLSRF")
#coords_wide.med.v3 <- result[[1]]
#coords_wide.sol.med.v3 <- result[[2]]

result <- generate_mean_values_from_netcdf("D:\\CoalFlyAsh\\no_soil_state_firex1_runs\\no_soil_state_an+du_cheyenne_bb_derecho.h2_v3.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v3 <- result[[1]]
coords_wide.sol.med.v3 <- result[[2]]
coords_wide.med.v3_sd <- result[[3]]
coords_wide.sol.med.v3_sd <- result[[4]]

# v4 (PD) INDCOAL0.05-RESICOAL33-WOOD56-OIL25-FINEFIRE33
# Cheyenne version
#result <- read_in_netcdf("D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.05-RESICOAL33-WOOD56-OIL25_2009-2011.h2.BBx1_ANx2_DUx2.nc", "FETOTSRF", "FESOLSRF")
#coords_wide.med.v4 <- result[[1]]
#coords_wide.sol.med.v4 <- result[[2]]

result <- generate_mean_values_from_netcdf("D:\\CoalFlyAsh\\no_soil_state_firex1_runs\\no_soil_state_an+du_cheyenne_bb_derecho.h2_v4.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v4 <- result[[1]]
coords_wide.sol.med.v4 <- result[[2]]
coords_wide.med.v4_sd <- result[[3]]
coords_wide.sol.med.v4_sd <- result[[4]]

# get dimensions of netcdf files to store for later ----------------------------------------
dim.file <- "D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38_2009-2011.h2.BBx1_ANx2_DUx2.nc"
# dim file for Cheyenne run
#dim.file <- "D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38_2009-2011MEAN.nc"
nc_data <- nc_open(dim.file)
# Extract latitude and longitude options
all_lon_options <- ncvar_get(nc_data, "lon")
all_lat_options <- ncvar_get(nc_data, "lat")
#all_lev_options <- ncvar_get(nc_data, "lev") 
all_time_options <- ncvar_get(nc_data, "time")
nc_close(nc_data)

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- abs(all_lat_options[2] - all_lat_options[1])  # Latitude grid spacing in degrees
d.lon <- abs(all_lon_options[2] - all_lon_options[1])  # Longitude grid spacing in degrees
d.time <- abs(all_time_options[2] - all_time_options[1])

# Simplifying matching of observational to modeled data
library(purrr)

# INDIVIDUAL COMPARISONS OBS TO MOD -- stick to medians when aggregating
obs.data.ind <- obs.data %>%
  mutate(Model_Latitude_Values = map_dbl(Latitude, ~ all_lat_options[which.min(abs(all_lat_options - .x))]))
obs.data.mod.res <- obs.data.ind %>%
  mutate(Model_Longitude_Values = map_dbl(Longitude, ~ all_lon_options[which.min(abs(all_lon_options - .x))]))

obs.data.mod.res.ind <- obs.data.mod.res %>%
  mutate(`Fe (ng m–3)_median` = `Fe (ng m–3)`,
         `Fe (ng m–3)_sd` = 0, 
         `labile Fe (ng m–3)_median` = `labile Fe (ng m–3)`,
         `labile Fe (ng m–3)_sd` = 0,
         `Fe solubility_median` = `Fe solubility`,
         `Fe solubility_sd` = 0) %>%
  dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`, `Fe (ng m–3)_sd`, `labile Fe (ng m–3)_median`, `labile Fe (ng m–3)_sd`, `Fe solubility_median`, `Fe solubility_sd`) %>%
  rename_with(~ paste0(.x, "_ind"), 
              .cols = !c("Model_Latitude_Values", "Model_Longitude_Values"))

# n1 AGGREGATED BY GRID CELL (1x1)
obs.data.mod.res.n1 <- obs.data.mod.res %>%
    mutate(Model_Latitude_Values = as.character(Model_Latitude_Values),
           Model_Longitude_Values = as.character(Model_Longitude_Values)) %>%
    group_by(Model_Latitude_Values, Model_Longitude_Values) %>%
    summarise(across(c("Fe (ng m–3)", "Fe solubility", "labile Fe (ng m–3)"), 
                     list(median = median, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values),
           Model_Longitude_Values = as.numeric(Model_Longitude_Values)) 

# %>% rename_with(~ paste0(.x, "_n1"),  .cols = !c("Model_Latitude_Values", "Model_Longitude_Values"))
# instead just toggle here depending on aggregation area

# n4 AGGREGATED BY GRID CELL (2X2)
n4_lat_options <- all_lat_options[seq(1, length(all_lat_options), by = 2)]
n4_lon_options <- all_lon_options[seq(1, length(all_lon_options), by = 2)]

obs.data.n4 <- obs.data %>%
  mutate(Model_Latitude_Values = map_dbl(Latitude, ~ n4_lat_options[which.min(abs(n4_lat_options - .x))]))
obs.data.mod.res.n4 <- obs.data.n4 %>%
  mutate(Model_Longitude_Values = map_dbl(Longitude, ~ n4_lon_options[which.min(abs(n4_lon_options - .x))]))

obs.data.mod.res.n4 <- obs.data.mod.res.n4 %>%
  mutate(Model_Latitude_Values = as.character(Model_Latitude_Values),
         Model_Longitude_Values = as.character(Model_Longitude_Values)) %>%
  group_by(Model_Latitude_Values, Model_Longitude_Values) %>%
  summarise(across(c("Fe (ng m–3)", "Fe solubility", "labile Fe (ng m–3)"), 
                   list(median = median, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values),
         Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>%
  rename_with(~ paste0(.x, "_n4"), 
              .cols = !c("Model_Latitude_Values", "Model_Longitude_Values"))

# combine observational with modeled data ----------------------------------
library(tibble)
# converting Lat and Lon to character variables
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values) 
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values) 

# TOTAL FE
obs.data.mod.res.n1 <- obs.data.mod.res.n1 %>%
  mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values))
    
# v1 ------
coords_long.med.v1_median <- coords_wide.med.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_Fe = value)

coords_long.med.v1_sd <- coords_wide.med.v1_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_Fe_sd = value)

coords_long.med.v1 <- full_join(coords_long.med.v1_median, coords_long.med.v1_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v1_Fe, v1_Fe_sd, everything())

# v2 ------
coords_long.med.v2_median <- coords_wide.med.v2 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v2_Fe = value)

coords_long.med.v2_sd <- coords_wide.med.v2_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v2_Fe_sd = value)

coords_long.med.v2 <- full_join(coords_long.med.v2_median, coords_long.med.v2_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v2_Fe, v2_Fe_sd, everything())

# v3 ------
coords_long.med.v3_median <- coords_wide.med.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v3_Fe = value)

coords_long.med.v3_sd <- coords_wide.med.v3_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v3_Fe_sd = value)

coords_long.med.v3 <- full_join(coords_long.med.v3_median, coords_long.med.v3_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v3_Fe, v3_Fe_sd, everything())

# v4 ------
coords_long.med.v4_median <- coords_wide.med.v4 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v4_Fe = value)

coords_long.med.v4_sd <- coords_wide.med.v4_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v4_Fe_sd = value)

coords_long.med.v4 <- full_join(coords_long.med.v4_median, coords_long.med.v4_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v4_Fe, v4_Fe_sd, everything())

# COMPILING TOTAL FE MODELED RESULTS -----
all.TOTFe <- full_join(coords_long.med.v1, coords_long.med.v2) %>% 
  full_join(coords_long.med.v3) %>% 
  full_join(coords_long.med.v4)   

# SOLUBLE FE 

# v1 --------
coords_long.sol.med.v1_median <- coords_wide.sol.med.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_sol_Fe = value)

coords_long.sol.med.v1_sd <- coords_wide.sol.med.v1_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_sol_Fe_sd = value)

coords_long.sol.med.v1 <- full_join(coords_long.sol.med.v1_median, coords_long.sol.med.v1_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v1_sol_Fe, v1_sol_Fe_sd, everything())

# v2 --------
coords_long.sol.med.v2_median <- coords_wide.sol.med.v2 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v2_sol_Fe = value)

coords_long.sol.med.v2_sd <- coords_wide.sol.med.v2_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v2_sol_Fe_sd = value)

coords_long.sol.med.v2 <- full_join(coords_long.sol.med.v2_median, coords_long.sol.med.v2_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v2_sol_Fe, v2_sol_Fe_sd, everything())

# v3 --------
coords_long.sol.med.v3_median <- coords_wide.sol.med.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v3_sol_Fe = value)

coords_long.sol.med.v3_sd <- coords_wide.sol.med.v3_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v3_sol_Fe_sd = value)

coords_long.sol.med.v3 <- full_join(coords_long.sol.med.v3_median, coords_long.sol.med.v3_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v3_sol_Fe, v3_sol_Fe_sd, everything())

# v4 --------
coords_long.sol.med.v4_median <- coords_wide.sol.med.v4 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v4_sol_Fe = value)

coords_long.sol.med.v4_sd <- coords_wide.sol.med.v4_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v4_sol_Fe_sd = value)

coords_long.sol.med.v4 <- full_join(coords_long.sol.med.v4_median, coords_long.sol.med.v4_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v4_sol_Fe, v4_sol_Fe_sd, everything())


# COMPILING ALL SOLUBLE IRON DATA ---------
all.SOLFe <- full_join(coords_long.sol.med.v1, coords_long.sol.med.v2) %>% 
  full_join(coords_long.sol.med.v3) %>% 
  full_join(coords_long.sol.med.v4) 

# merging all coal fly ash data frames for easier comparisons  --------------------------------
# version with population medians 
coal.ash.sims <- left_join(all.TOTFe, all.SOLFe) %>% 
  rename(Latitude = Model_Latitude_Values, 
         Longitude = Model_Longitude_Values, 
         Obs_Fe = `Fe (ng m–3)_median`, 
         Obs_sol_Fe = `labile Fe (ng m–3)_median`, 
         Obs_solubility = `Fe solubility_median`, 
         Obs_Fe_sd = `Fe (ng m–3)_sd`,
         Obs_sol_Fe_sd = `labile Fe (ng m–3)_sd`) %>%
  
  # add this when applying dust AOD cf for soilstate runs
#  mutate(v1_Fe = v1_Fe*3.8, v1_sol_Fe = v1_sol_Fe*3.8, 
   #      v2_Fe = v2_Fe*3.8, v2_sol_Fe = v2_sol_Fe*3.8,
     #    v3_Fe = v3_Fe*3.8, v3_sol_Fe = v3_sol_Fe*3.8,
     #    v4_Fe = v4_Fe*3.8, v4_sol_Fe = v4_sol_Fe*3.8) %>% 
  
  mutate(v1_solubility = v1_sol_Fe / v1_Fe, 
         v2_solubility = v2_sol_Fe / v2_Fe, 
         v3_solubility = v3_sol_Fe / v3_Fe, 
         v4_solubility = v4_sol_Fe / v4_Fe, 
         v1_solubility_sd = v1_sol_Fe_sd / v1_Fe_sd, 
         v2_solubility_sd = v2_sol_Fe_sd / v2_Fe_sd, 
         v3_solubility_sd = v3_sol_Fe_sd / v3_Fe_sd, 
         v4_solubility_sd = v4_sol_Fe_sd / v4_Fe_sd) 
        

# adding geographical regions by latitude and longitude ------------------------------------------
coal.ash.sims$Latitude <- as.numeric(coal.ash.sims$Latitude)
coal.ash.sims$Longitude <- as.numeric(coal.ash.sims$Longitude)

# Regional 4 grouping has been updated as of 1-15-2025 and is cross referencing python script
coal.ash.sims <- coal.ash.sims %>% 
  mutate(region_4 = case_when(
    (Longitude >= 295 & Latitude <= -15 & Latitude > -48) | (Longitude < 30 & Latitude <= -15 & Latitude >= -48) ~ "SATL",
    (Longitude > 265 & Latitude > 29 & Latitude < 60) | (Longitude < 110 & Latitude > 29 & Latitude < 60) ~ "NATL",
    Longitude < 78 & Longitude >= 30 & Latitude <= 29 & Latitude > -48 ~ "AS",
    Longitude < 110 & Longitude >= 78 & Latitude <= 29 & Latitude > -48 ~ "BB",
    Longitude < 110 & Longitude >= 30 & Latitude <= 29 & Latitude > -48 ~ "INDO",
    Longitude < 150 & Longitude >= 110 & Latitude < 60 & Latitude > -15 ~ "SEAS",
   # Longitude <= 265 & Longitude >= 150 & Latitude < 60 & Latitude > 29 ~ "NPAC",
    Longitude <= 180 & Longitude >= 150 & Latitude < 60 & Latitude > 29 ~ "ENPAC",
    Longitude <= 265 & Longitude > 180 & Latitude < 60 & Latitude > 29 ~ "WNPAC",
    Latitude >= 60 & Longitude >= 0 ~ "ARCT" ,
    Longitude < 295 & Longitude > 110 & Latitude <= -15 & Latitude >= -48 ~ "AUSP",
    Longitude >= 0 & Latitude <= -48 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > -15 & Latitude <= 29) | (Longitude < 30 & Latitude > -15 & Latitude <= 29) ~ "CPAO"))

# Color palettes  -------------------- 
# set color palette
library(RColorBrewer);library(ggbreak) 

India.palette <- c("ARCT" = "#1B9E7780",
                   "SO"   = "#1F78B480" ,
                   "NATL" = "#7570B380",
                   "SATL" = "#E7298A80",
                   "CPAO" = "#66A61E80" , 
                   "NPAC" = "#E6AB0280" , 
                   "ENPAC" = "#c4a13b80" ,
                   "WNPAC" = "#ffd14d80" ,
                   "SEAS" = "#8d6c2f80", 
                   "AUSP" = "#66666680", 
                   "BB" = "#D95F0280", 
                   "AS" = "#2d519280", 
                   "INDO" = "#FF7F0080")

India.shapes <- c("ARCT" = 16,
                   "SO"   = 16,
                   "NATL" = 16,
                   "SATL" = 16,
                   "CPAO" = 16, 
                   "NPAC" = 16, 
                   "ENPAC" = 18,
                   "WNPAC" = 18,
                   "SEAS" = 16, 
                   "AUSP" = 16, 
                   "BB" = 18, 
                   "AS" = 18, 
                   "INDO" = 16)

na_count <- sum(is.na(coal.ash.sims$region_4))
print(na_count)
region_counts <- table(coal.ash.sims$region_4)
print(region_counts)

# Histogram of observations per each region 
ggplot(coal.ash.sims, aes(x = region_4, fill = region_4, position = "identity")) +
  geom_bar(stat = "count") +
  labs(title = "Histogram of Values", x = "Value", y = "Frequency") +
  theme_minimal() + 
  scale_fill_manual(values = India.palette) + 
  coord_flip() 

# Create a new column that combines AS and BB into INDO and E and W NPAC into NPAC
coal.ash.sims$region_4_combined <- coal.ash.sims$region_4
coal.ash.sims$region_4_combined[coal.ash.sims$region_4 %in% c("AS", "BB")] <- "INDO"
coal.ash.sims$region_4_combined[coal.ash.sims$region_4 %in% c("ENPAC", "WNPAC")] <- "NPAC"

# Create the plot
ggplot(coal.ash.sims, aes(x = region_4_combined, fill = region_4_combined)) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Histogram of Values", x = "Region", y = "Frequency") +
  theme_minimal() + 
  scale_fill_manual(values = India.palette) +
  coord_flip() +
  theme(legend.title = element_blank())  # Optional: remove legend title

# ------------------------------------- VISUALIZATIONS --------------------------------------------
# Linear Regression plots comparing obs and modeled values independent of region on a log scale 
# making single points visualized in regression for easier interpretation ------------------------------
region_count <- coal.ash.sims %>% group_by(region_4) %>% summarize(count = n())

# ADDING DETAILS FOR INDO AND NPAC COMBINED
coal.ash.sims.1 <- coal.ash.sims %>% 
  mutate(across(-c(region_4, region_4_combined), as.numeric)) %>%
  dplyr::select(-Latitude, -Longitude, -region_4_combined) %>%
  group_by(region_4) %>%
  summarize(across(everything(), mean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  full_join(region_count) %>% 
  mutate(count = as.numeric(count)) %>% drop_na()

indo_row <- coal.ash.sims.1 %>%
  filter(region_4 %in% c("BB", "AS")) %>%
  dplyr::select(-matches(c("count"))) %>% 
  summarize(across(-region_4, mean, na.rm = TRUE)) %>%
  mutate(region_4 = "INDO") %>% 
  mutate(Obs_solubility = Obs_sol_Fe/Obs_Fe, 
         v1_solubility = v1_sol_Fe / v1_Fe, 
         v2_solubility = v2_sol_Fe / v2_Fe, 
         v3_solubility = v3_sol_Fe / v3_Fe, 
         v4_solubility = v4_sol_Fe / v4_Fe) %>%
 mutate(count = 99)

npac_row <- coal.ash.sims.1 %>%
  filter(region_4 %in% c("ENPAC", "WNPAC")) %>%
  dplyr::select(-matches(c("count"))) %>% 
  summarize(across(-region_4, mean, na.rm = TRUE)) %>%
  mutate(region_4 = "NPAC") %>% 
  mutate(Obs_solubility = Obs_sol_Fe/Obs_Fe, 
         v1_solubility = v1_sol_Fe / v1_Fe, 
         v2_solubility = v2_sol_Fe / v2_Fe, 
         v3_solubility = v3_sol_Fe / v3_Fe, 
         v4_solubility = v4_sol_Fe / v4_Fe) %>%
  mutate(count = 87)

# Add the new INDO row to the original dataset
coal.ash.sims.1 <- bind_rows(coal.ash.sims.1, indo_row, npac_row) %>% filter(region_4 != "ARCT", region_4 != "NA")

# need to add standard deviation for obs and model 
coal.ash.sims.2 <- coal.ash.sims.1 %>%
  mutate(xmin = (Obs_Fe-Obs_Fe_sd), xmax = (Obs_Fe+Obs_Fe_sd),
         ymin_v1 = (v1_Fe-v1_Fe_sd), ymax_v1 = (v1_Fe+v1_Fe_sd),
         ymin_v2 = (v2_Fe-v2_Fe_sd), ymax_v2 = (v2_Fe+v2_Fe_sd),
         ymin_v3 = (v3_Fe-v3_Fe_sd), ymax_v3 = (v3_Fe+v3_Fe_sd),
         ymin_v4 = (v4_Fe-v4_Fe_sd), ymax_v4 = (v4_Fe+v4_Fe_sd),
      
         xmin_sol = (Obs_sol_Fe-Obs_sol_Fe_sd), xmax_sol = (Obs_sol_Fe+Obs_sol_Fe_sd),
         ymin_sol_v1 = (v1_sol_Fe-v1_sol_Fe_sd), ymax_sol_v1 = (v1_sol_Fe+v1_sol_Fe_sd),
         ymin_sol_v2 = (v2_sol_Fe-v2_sol_Fe_sd), ymax_sol_v2 = (v2_sol_Fe+v2_sol_Fe_sd),
         ymin_sol_v3 = (v3_sol_Fe-v3_sol_Fe_sd), ymax_sol_v3 = (v3_sol_Fe+v3_sol_Fe_sd),
         ymin_sol_v4 = (v4_sol_Fe-v4_sol_Fe_sd), ymax_sol_v4 = (v4_sol_Fe+v4_sol_Fe_sd),
    
         xmin_solubility = (Obs_solubility-`Fe solubility_sd`), xmax_solubility = (Obs_solubility+`Fe solubility_sd`),
         ymin_solubility_v1 = (v1_solubility-v1_solubility_sd), ymax_solubility_v1 = (v1_solubility+v1_solubility_sd),
         ymin_solubility_v2 = (v2_solubility-v2_solubility_sd), ymax_solubility_v2 = (v2_solubility+v2_solubility_sd),
         ymin_solubility_v3 = (v3_solubility-v3_solubility_sd), ymax_solubility_v3 = (v3_solubility+v3_solubility_sd),
         ymin_solubility_v4 = (v4_solubility-v4_solubility_sd), ymax_solubility_v4 = (v4_solubility+v4_solubility_sd)) %>% 
  mutate(across(everything(), ~ ifelse(. <= 0, 0.000001, .)))

coal.ash.sims.no.npac <- coal.ash.sims.2 %>% filter(region_4 != "NPAC")
  
create_soluble_iron_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, x_label = "Observed Soluble [Fe] (ng m-3)", y_label = "Modeled Soluble [Fe] v10 (ng m-3)", plot_title = "Observed vs. Modeled Soluble Iron") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = region, size = count_col)) +
    geom_point(aes(shape = region_4)) +  # Add points
    scale_color_manual(values = color_palette) +
    scale_shape_manual(values = India.shapes) +
    scale_size(range = c(3, 20)) +
    theme_minimal() + 
    scale_x_log10(labels = function(x) format(x, scientific = FALSE)) + 
    scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + 
    coord_cartesian(ylim = c(0.1, 100), xlim = c(0.1, 100)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    theme(legend.position = "none")
}

create_soluble_iron_plot_single_points_v2 <- function(data, x_var, y_var, x_min, x_max, y_min, y_max, region, color_palette, count_col, x_label = "Observed Soluble [Fe] (ng m-3)", y_label = "Modeled Soluble [Fe] v1 (ng m-3)", plot_title = "Observed vs. Modeled Soluble Iron") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    
    geom_errorbar(aes(x = .data[[x_var]], ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.6, color = "gray") +
    geom_errorbar(aes(y = .data[[y_var]], xmin = .data[[x_min]], xmax = .data[[x_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.6, color = "gray") +
    
    # Scatter points with dynamic aes mapping using .data[[...]] for variables
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], 
                   color = .data[[region]], size = .data[[count_col]], 
                   shape = .data[["region_4"]])) + 
    
    scale_color_manual(values = color_palette) +
    scale_size(range = c(4, 20)) +
    scale_shape_manual(values = India.shapes) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.001)) + 
    scale_y_log10(labels = scales::number_format(accuracy = 0.001)) +
    coord_cartesian(ylim = c(0.01, 100), xlim = c(0.01, 100)) +  
    
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    
    labs(x = x_label, y = y_label, title = plot_title)
}

create_total_iron_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, x_label = "Observed [Fe] (ng m-3)",  y_label = "Modeled [Fe] (ng m-3)", plot_title = "Observed vs. Modeled Total Iron") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = region, size = count_col)) +
    geom_point(aes(shape = region_4)) +  # Add points
    scale_color_manual(values = color_palette) +
    scale_shape_manual(values = India.shapes) +
    scale_size(range = c(3, 20)) +
    theme_minimal() + 
    scale_x_log10(labels = scales::scientific_format(digits = 1)) +  # Scientific notation for x-axis
    scale_y_log10(labels = scales::scientific_format(digits = 1)) + 
    coord_cartesian(ylim = c(1, 10000), xlim = c(1, 10000)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    theme(legend.position = "none")
}

create_total_iron_plot_single_points_v2 <- function(data, x_var, y_var, x_min, x_max, y_min, y_max, region, color_palette, count_col, x_label = "Observed Total [Fe] (ng m-3)", y_label = "Modeled Total [Fe] v1 (ng m-3)", plot_title = "Observed vs. Modeled Total Iron") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    
    geom_errorbar(aes(x = .data[[x_var]], ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.6, color = "gray") +
    geom_errorbar(aes(y = .data[[y_var]], xmin = .data[[x_min]], xmax = .data[[x_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.6, color = "gray") +
    
    # Scatter points with dynamic aes mapping using .data[[...]] for variables
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], 
                   color = .data[[region]], size = .data[[count_col]], 
                   shape = .data[["region_4"]])) + 
    
    scale_color_manual(values = color_palette) +
    scale_size(range = c(4, 20)) +
    scale_shape_manual(values = India.shapes) +
    scale_x_log10(labels = scales::scientific_format(digits = 1)) + 
    scale_y_log10(labels = scales::scientific_format(digits = 1)) +
    coord_cartesian(ylim = c(.1, 10000), xlim = c(.1, 10000)) +  
    
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    
    labs(x = x_label, y = y_label, title = plot_title)
}

create_solubility_plot_single_points <- function(data, x_var, y_var, region, color_palette, count_col, x_label = "Observed Fractional Fe Solubility (%)", y_label = "Modeled Fractional Fe Solubility (%)", plot_title = "Observed vs. Modeled Fe Solubility") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = region, size = count_col)) +
    geom_point(aes(shape = region_4)) +  # Add points
    scale_color_manual(values = color_palette) +
    scale_size(range = c(3, 20)) +
    scale_shape_manual(values = India.shapes) +
    theme_minimal() + 
    scale_x_log10(labels = scales::number_format(accuracy = 0.001)) + 
    scale_y_log10(labels = scales::number_format(accuracy = 0.001)) + 
    coord_cartesian(ylim = c(0.01, .15), xlim = c(0.01, .15)) +  # Adjusted limits to avoid log(0)
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    theme(legend.position = "none")
}


create_solubility_plot_single_points_v2 <- function(data, x_var, y_var, x_min, x_max, y_min, y_max, region, color_palette, count_col,
x_label = "Observed Fractional Fe Solubility (%)", y_label = "Modeled Fractional Fe Solubility (%)", plot_title = "Observed vs. Modeled Fe Solubility") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    
    geom_errorbar(aes(x = .data[[x_var]], ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.6, color = "gray") +
    geom_errorbar(aes(y = .data[[y_var]], xmin = .data[[x_min]], xmax = .data[[x_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.6, color = "gray") +
    
    # Scatter points with dynamic aes mapping using .data[[...]] for variables
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], 
                   color = .data[[region]], size = .data[[count_col]], 
                   shape = .data[["region_4"]])) + 
    
    scale_color_manual(values = color_palette) +
    scale_size(range = c(4, 20)) +
    scale_shape_manual(values = India.shapes) +
    scale_x_log10(labels = scales::scientific_format(digits = 1)) + 
    scale_y_log10(labels = scales::scientific_format(digits = 1)) +
    coord_cartesian(ylim = c(0.001, .2), xlim = c(0.001, .2)) +  
    
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    
    labs(x = x_label, y = y_label, title = plot_title)
}


# list out the following (df, "x_var", "y_var", "x_min", "x_max", "y_min", "y_max", region, color_palette, "count")
v1.dot.plot.sol <- create_soluble_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_sol_Fe", "v1_sol_Fe", "xmin_sol", "xmax_sol", "ymin_sol_v1", "ymax_sol_v1", "region_4", India.palette, "count")
v1.dot.plot.tot <- create_total_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_Fe", "v1_Fe", "xmin", "xmax", "ymin_v1", "ymax_v1", "region_4", India.palette,  "count")
v1.dot.plot.per.sol <- create_solubility_plot_single_points_v2(coal.ash.sims.2, "Obs_solubility", "v1_solubility", "xmin_solubility", "xmax_solubility", "ymin_solubility_v1", "ymax_solubility_v1", "region_4", India.palette, "count")

v2.dot.plot.sol <- create_soluble_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_sol_Fe", "v2_sol_Fe", "xmin_sol", "xmax_sol", "ymin_sol_v2", "ymax_sol_v2", "region_4", India.palette, "count")
v2.dot.plot.tot <- create_total_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_Fe", "v2_Fe", "xmin", "xmax", "ymin_v2", "ymax_v2", "region_4", India.palette,  "count")
v2.dot.plot.per.sol <- create_solubility_plot_single_points_v2(coal.ash.sims.2, "Obs_solubility", "v2_solubility", "xmin_solubility", "xmax_solubility", "ymin_solubility_v2", "ymax_solubility_v2", "region_4", India.palette, "count")

v3.dot.plot.sol <- create_soluble_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_sol_Fe", "v3_sol_Fe", "xmin_sol", "xmax_sol", "ymin_sol_v3", "ymax_sol_v3", "region_4", India.palette, "count")
v3.dot.plot.tot <- create_total_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_Fe", "v3_Fe", "xmin", "xmax", "ymin_v3", "ymax_v3", "region_4", India.palette,  "count")
v3.dot.plot.per.sol <- create_solubility_plot_single_points_v2(coal.ash.sims.2, "Obs_solubility", "v3_solubility", "xmin_solubility", "xmax_solubility", "ymin_solubility_v3", "ymax_solubility_v3", "region_4", India.palette, "count")

v4.dot.plot.sol <- create_soluble_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_sol_Fe", "v4_sol_Fe", "xmin_sol", "xmax_sol", "ymin_sol_v4", "ymax_sol_v4", "region_4", India.palette, "count")
v4.dot.plot.tot <- create_total_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_Fe", "v4_Fe", "xmin", "xmax", "ymin_v4", "ymax_v4", "region_4", India.palette,  "count")
v4.dot.plot.per.sol <- create_solubility_plot_single_points_v2(coal.ash.sims.2, "Obs_solubility", "v4_solubility", "xmin_solubility", "xmax_solubility", "ymin_solubility_v4", "ymax_solubility_v4", "region_4", India.palette, "count")

# PLOTS THEMSELVES -------------------
v1.dot.plot.tot 

v1.dot.plot.sol 
v2.dot.plot.sol 
v3.dot.plot.sol
v4.dot.plot.sol

v1.dot.plot.per.sol
v2.dot.plot.per.sol
v3.dot.plot.per.sol 
v4.dot.plot.per.sol

# calculating Error Metrics for regression analyses comparing Obs and Modeled values -------------------------
error_metrics <- function(df, observed, predicted) {
  # Dynamically reference columns
  observed_values <- df[[deparse(substitute(observed))]]
  predicted_values <- df[[deparse(substitute(predicted))]]
  
  # Remove NAs from both columns
  valid_cases <- complete.cases(observed_values, predicted_values)
  observed_values <- observed_values[valid_cases]
  predicted_values <- predicted_values[valid_cases]
  
  # Calculate metrics
  r_squared <- cor(observed_values, predicted_values)^2
  rmse <- sqrt(mean((observed_values - predicted_values)^2))
  mae <- mean(abs(observed_values - predicted_values))
  
  return(data.frame(R2 = r_squared, RMSE = rmse, MAE = mae))
}

# Apply the function to the coal.ash.sims dataframe
MIMI_metrics <- error_metrics(coal.ash.sims, Obs_sol_Fe, v1_sol_Fe)
MIMI_metrics_by_region <- coal.ash.sims %>%
  group_by(region_4) %>%
  summarise(
    error_metrics = list(error_metrics(cur_data(), Obs_sol_Fe, v1_sol_Fe)),
    .groups = 'drop'
  ) %>%
  unnest(cols = c(error_metrics))

v2_metrics <- error_metrics(coal.ash.sims, Obs_sol_Fe, v2_sol_Fe)
v2_metrics_by_region <- coal.ash.sims %>%
  group_by(region_4) %>%
  summarise(
    error_metrics = list(error_metrics(cur_data(), Obs_sol_Fe, v2_sol_Fe)),
    .groups = 'drop'
  ) %>%
  unnest(cols = c(error_metrics))

v3_metrics <- error_metrics(coal.ash.sims, Obs_sol_Fe, v3_sol_Fe)
v3_metrics_by_region <- coal.ash.sims %>%
  group_by(region_4) %>%
  summarise(
    error_metrics = list(error_metrics(cur_data(), Obs_sol_Fe, v3_sol_Fe)),
    .groups = 'drop'
  ) %>%
  unnest(cols = c(error_metrics))

v4_metrics <- error_metrics(coal.ash.sims, Obs_sol_Fe, v4_sol_Fe)
v4_metrics_by_region <- coal.ash.sims %>%
  group_by(region_4) %>%
  summarise(
    error_metrics = list(error_metrics(cur_data(), Obs_sol_Fe, v4_sol_Fe)),
    .groups = 'drop'
  ) %>%
  unnest(cols = c(error_metrics))

# need to go through and do this for the others that are important and possibly by region 


# Source apportioned stacked bar plots comparing FU, PI, and PD simulations -------------------------------------------------------
# specific aerosol sources -- these values are actually going to be DEPOSITION FLUXES FROM PYTHON 
# V1 ------------
#result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\CAM6-MIMI-PD-test-20250129_noSoilState.cam.h2.JAN.nc", "FEDUTOTSRF", "FEDUSOLSRF")
result_du <-generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v1 <- result_du[[1]]
coords_wide.sol.du.v1 <- result_du[[2]]
coords_wide.du.v1_sd <- result[[3]]
coords_wide.sol.du.v1_sd <- result[[4]]

#result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\CAM6-MIMI-PD-test-20250129_noSoilState.cam.h2.JAN.nc", "FEANTOTSRF", "FEANSOLSRF")
result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v1 <- result_an[[1]]
coords_wide.sol.an.v1 <- result_an[[2]]
coords_wide.an.v1_sd <- result[[3]]
coords_wide.sol.an.v1_sd <- result[[4]]

#result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\CAM6-MIMI-PD-test-20250129_noSoilState.cam.h2.JAN.nc", "FEBBTOTSRF", "FEBBSOLSRF")
result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v1 <- result_bb[[1]]
coords_wide.sol.bb.v1 <- result_bb[[2]]
coords_wide.bb.v1_sd <- result[[3]]
coords_wide.sol.bb.v1_sd <- result[[4]]

# V3 ----------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38.cam.h2.2009-2011_PD.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v3 <- result_du[[1]]
coords_wide.sol.du.v3 <- result_du[[2]]
coords_wide.du.v3_sd <- result[[3]]
coords_wide.sol.du.v3_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38.cam.h2.2009-2011_PD.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v3 <- result_an[[1]]
coords_wide.sol.an.v3 <- result_an[[2]]
coords_wide.an.v3_sd <- result[[3]]
coords_wide.sol.an.v3_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL33-WOOD56-OIL38.cam.h2.2009-2011_PD.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v3 <- result_bb[[1]]
coords_wide.sol.bb.v3 <- result_bb[[2]]
coords_wide.bb.v3_sd <- result[[3]]
coords_wide.sol.bb.v3_sd <- result[[4]]

# V6 ----------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\FHIST_transient_test.cam.h2.2008-JAN.nc", "FEDUTOTSRF", "FEDUSOLSRF")
# result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE33.cam.h2.2009-2011_PI.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v6 <- result_du[[1]]
coords_wide.sol.du.v6 <- result_du[[2]]
coords_wide.du.v6_sd <- result[[3]]
coords_wide.sol.du.v6_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\FHIST_transient_test.cam.h2.2008-JAN.nc", "FEANTOTSRF", "FEANSOLSRF")
#result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE33.cam.h2.2009-2011_PI.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v6 <- result_an[[1]]
coords_wide.sol.an.v6 <- result_an[[2]]
coords_wide.an.v6_sd <- result[[3]]
coords_wide.sol.an.v6_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Transient Run\\Test outputs\\FHIST_transient_test.cam.h2.2008-JAN.nc", "FEBBTOTSRF", "FEBBSOLSRF")
#result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL0.2-FIRE33.cam.h2.2009-2011_PI.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v6 <- result_bb[[1]]
coords_wide.sol.bb.v6 <- result_bb[[2]]
coords_wide.bb.v6_sd <- result[[3]]
coords_wide.sol.bb.v6_sd <- result[[4]]

# V7 -------------------------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE33_v2.cam.h2.2009-2011.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v7 <- result_du[[1]]
coords_wide.sol.du.v7 <- result_du[[2]]
coords_wide.du.v7_sd <- result[[3]]
coords_wide.sol.du.v7_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE33_v2.cam.h2.2009-2011.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v7 <- result_an[[1]]
coords_wide.sol.an.v7 <- result_an[[2]]
coords_wide.an.v7_sd <- result[[3]]
coords_wide.sol.an.v7_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-PI-RESICOAL33_FINEFIRE33_v2.cam.h2.2009-2011.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v7 <- result_bb[[1]]
coords_wide.sol.bb.v7 <- result_bb[[2]]
coords_wide.bb.v7_sd <- result[[3]]
coords_wide.sol.bb.v7_sd <- result[[4]]

# V8 -------------------------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v8 <- result_du[[1]]
coords_wide.sol.du.v8 <- result_du[[2]]
coords_wide.du.v8_sd <- result[[3]]
coords_wide.sol.du.v8_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v8 <- result_an[[1]]
coords_wide.sol.an.v8 <- result_an[[2]]
coords_wide.an.v8_sd <- result[[3]]
coords_wide.sol.an.v8_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v8 <- result_bb[[1]]
coords_wide.sol.bb.v8 <- result_bb[[2]]
coords_wide.bb.v8_sd <- result[[3]]
coords_wide.sol.bb.v8_sd <- result[[4]]

# V9 -------------------------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v9 <- result_du[[1]]
coords_wide.sol.du.v9 <- result_du[[2]]
coords_wide.du.v9_sd <- result[[3]]
coords_wide.sol.du.v9_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v9 <- result_an[[1]]
coords_wide.sol.an.v9 <- result_an[[2]]
coords_wide.an.v9_sd <- result[[3]]
coords_wide.sol.an.v9_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v9 <- result_bb[[1]]
coords_wide.sol.bb.v9 <- result_bb[[2]]
coords_wide.bb.v9_sd <- result[[3]]
coords_wide.sol.bb.v9_sd <- result[[4]]

# V10 -------------------------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v10 <- result_du[[1]]
coords_wide.sol.du.v10 <- result_du[[2]]
coords_wide.du.v10_sd <- result[[3]]
coords_wide.sol.du.v10_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v10 <- result_an[[1]]
coords_wide.sol.an.v10 <- result_an[[2]]
coords_wide.an.v10_sd <- result[[3]]
coords_wide.sol.an.v10_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v10 <- result_bb[[1]]
coords_wide.sol.bb.v10 <- result_bb[[2]]
coords_wide.bb.v10_sd <- result[[3]]
coords_wide.sol.bb.v10_sd <- result[[4]]

# V11 -------------------------------------------
result_du <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEDUTOTSRF", "FEDUSOLSRF")
coords_wide.du.v11 <- result_du[[1]]
coords_wide.sol.du.v11 <- result_du[[2]]
coords_wide.du.v11_sd <- result[[3]]
coords_wide.sol.du.v11_sd <- result[[4]]

result_an <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEANTOTSRF", "FEANSOLSRF")
coords_wide.an.v11 <- result_an[[1]]
coords_wide.sol.an.v11 <- result_an[[2]]
coords_wide.an.v11_sd <- result[[3]]
coords_wide.sol.an.v11_sd <- result[[4]]

result_bb <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-SSP370-INDCOAL0.2-RESICOAL33-WOOD56-OIL38-FIRE33.cam.h2.2009-2011.nc", "FEBBTOTSRF", "FEBBSOLSRF")
coords_wide.bb.v11 <- result_bb[[1]]
coords_wide.sol.bb.v11 <- result_bb[[2]]
coords_wide.bb.v11_sd <- result[[3]]
coords_wide.sol.bb.v11_sd <- result[[4]]

# For DU, AN, and BB specifically ------------
# PD V1
coords_long.sol.du.v1_median <- coords_wide.sol.du.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v1_sol_du_Fe = value)

coords_long.sol.du.v1_sd <- coords_wide.sol.du.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v1_sol_du_Fe_sd = value)

coords_long.sol.an.v1_median <- coords_wide.sol.an.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v1_sol_an_Fe = value)

coords_long.sol.an.v1_sd <- coords_wide.sol.an.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v1_sol_an_Fe_sd = value)

coords_long.sol.bb.v1_median <- coords_wide.sol.bb.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v1_sol_bb_Fe = value)

coords_long.sol.bb.v1_sd <- coords_wide.sol.bb.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v1_sol_bb_Fe_sd = value)

all.v1.SOL <- full_join(coords_long.sol.du.v1_median, coords_long.sol.du.v1_sd) %>% 
  full_join(coords_long.sol.an.v1_median) %>% 
  full_join(coords_long.sol.an.v1_sd) %>% 
  full_join(coords_long.sol.bb.v1_median) %>% 
  full_join(coords_long.sol.bb.v1_sd)

# PD V3
coords_long.sol.du.v3_median <- coords_wide.sol.du.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v3_sol_du_Fe = value)

coords_long.sol.du.v3_sd <- coords_wide.sol.du.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v3_sol_du_Fe_sd = value)

coords_long.sol.an.v3_median <- coords_wide.sol.an.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v3_sol_an_Fe = value)

coords_long.sol.an.v3_sd <- coords_wide.sol.an.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v3_sol_an_Fe_sd = value)

coords_long.sol.bb.v3_median <- coords_wide.sol.bb.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v3_sol_bb_Fe = value)

coords_long.sol.bb.v3_sd <- coords_wide.sol.bb.v3 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v3_sol_bb_Fe_sd = value)

all.v3.SOL <- full_join(coords_long.sol.du.v3_median, coords_long.sol.du.v3_sd) %>% 
  full_join(coords_long.sol.an.v3_median) %>% 
  full_join(coords_long.sol.an.v3_sd) %>% 
  full_join(coords_long.sol.bb.v3_median) %>% 
  full_join(coords_long.sol.bb.v3_sd)

# PI v6
coords_long.sol.du.v6_median <- coords_wide.sol.du.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v6_sol_du_Fe = value)

coords_long.sol.du.v6_sd <- coords_wide.sol.du.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v6_sol_du_Fe_sd = value)

coords_long.sol.an.v6_median <- coords_wide.sol.an.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v6_sol_an_Fe = value)

coords_long.sol.an.v6_sd <- coords_wide.sol.an.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v6_sol_an_Fe_sd = value)

coords_long.sol.bb.v6_median <- coords_wide.sol.bb.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v6_sol_bb_Fe = value)

coords_long.sol.bb.v6_sd <- coords_wide.sol.bb.v6 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v6_sol_bb_Fe_sd = value)

all.v6.SOL <- full_join(coords_long.sol.du.v6_median, coords_long.sol.du.v6_sd) %>% 
  full_join(coords_long.sol.an.v6_median) %>% 
  full_join(coords_long.sol.an.v6_sd) %>% 
  full_join(coords_long.sol.bb.v6_median) %>% 
  full_join(coords_long.sol.bb.v6_sd)

# PI v7
coords_long.sol.du.v7_median <- coords_wide.sol.du.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v7_sol_du_Fe = value)

coords_long.sol.du.v7_sd <- coords_wide.sol.du.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v7_sol_du_Fe_sd = value)

coords_long.sol.an.v7_median <- coords_wide.sol.an.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v7_sol_an_Fe = value)

coords_long.sol.an.v7_sd <- coords_wide.sol.an.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v7_sol_an_Fe_sd = value)

coords_long.sol.bb.v7_median <- coords_wide.sol.bb.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v7_sol_bb_Fe = value)

coords_long.sol.bb.v7_sd <- coords_wide.sol.bb.v7 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v7_sol_bb_Fe_sd = value)

all.v7.SOL <- full_join(coords_long.sol.du.v7_median, coords_long.sol.du.v7_sd) %>% 
  full_join(coords_long.sol.an.v7_median) %>% 
  full_join(coords_long.sol.an.v7_sd) %>% 
  full_join(coords_long.sol.bb.v7_median) %>% 
  full_join(coords_long.sol.bb.v7_sd)

# FU v8
coords_long.sol.du.v8_median <- coords_wide.sol.du.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v8_sol_du_Fe = value)

coords_long.sol.du.v8_sd <- coords_wide.sol.du.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v8_sol_du_Fe_sd = value)

coords_long.sol.an.v8_median <- coords_wide.sol.an.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v8_sol_an_Fe = value)

coords_long.sol.an.v8_sd <- coords_wide.sol.an.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v8_sol_an_Fe_sd = value)

coords_long.sol.bb.v8_median <- coords_wide.sol.bb.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v8_sol_bb_Fe = value)

coords_long.sol.bb.v8_sd <- coords_wide.sol.bb.v8 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v8_sol_bb_Fe_sd = value)

all.v8.SOL <- full_join(coords_long.sol.du.v8_median, coords_long.sol.du.v8_sd) %>% 
  full_join(coords_long.sol.an.v8_median) %>% 
  full_join(coords_long.sol.an.v8_sd) %>% 
  full_join(coords_long.sol.bb.v8_median) %>% 
  full_join(coords_long.sol.bb.v8_sd)

# FU v9
coords_long.sol.du.v9_median <- coords_wide.sol.du.v9 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v9_sol_du_Fe = value)

coords_long.sol.du.v9_sd <- coords_wide.sol.du.v9 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v9_sol_du_Fe_sd = value)

coords_long.sol.an.v9_median <- coords_wide.sol.an.v9 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v9_sol_an_Fe = value)

coords_long.sol.an.v9_sd <- coords_wide.sol.an.v9 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v9_sol_an_Fe_sd = value)

coords_long.sol.bb.v9_median <- coords_wide.sol.bb.v9 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v9_sol_bb_Fe = value)

coords_long.sol.bb.v9_sd <- coords_wide.sol.bb.v9 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v9_sol_bb_Fe_sd = value)

all.v9.SOL <- full_join(coords_long.sol.du.v9_median, coords_long.sol.du.v9_sd) %>% 
  full_join(coords_long.sol.an.v9_median) %>% 
  full_join(coords_long.sol.an.v9_sd) %>% 
  full_join(coords_long.sol.bb.v9_median) %>% 
  full_join(coords_long.sol.bb.v9_sd)

# FU v10
coords_long.sol.du.v10_median <- coords_wide.sol.du.v10 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v10_sol_du_Fe = value)

coords_long.sol.du.v10_sd <- coords_wide.sol.du.v10 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v10_sol_du_Fe_sd = value)

coords_long.sol.an.v10_median <- coords_wide.sol.an.v10 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v10_sol_an_Fe = value)

coords_long.sol.an.v10_sd <- coords_wide.sol.an.v10 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v10_sol_an_Fe_sd = value)

coords_long.sol.bb.v10_median <- coords_wide.sol.bb.v10 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v10_sol_bb_Fe = value)

coords_long.sol.bb.v10_sd <- coords_wide.sol.bb.v10 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v10_sol_bb_Fe_sd = value)

all.v10.SOL <- full_join(coords_long.sol.du.v10_median, coords_long.sol.du.v10_sd) %>% 
  full_join(coords_long.sol.an.v10_median) %>% 
  full_join(coords_long.sol.an.v10_sd) %>% 
  full_join(coords_long.sol.bb.v10_median) %>% 
  full_join(coords_long.sol.bb.v10_sd)

# FU V11
coords_long.sol.du.v11_median <- coords_wide.sol.du.v11 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v11_sol_du_Fe = value)

coords_long.sol.du.v11_sd <- coords_wide.sol.du.v11 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v11_sol_du_Fe_sd = value)

coords_long.sol.an.v11_median <- coords_wide.sol.an.v11 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v11_sol_an_Fe = value)

coords_long.sol.an.v11_sd <- coords_wide.sol.an.v11 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v11_sol_an_Fe_sd = value)

coords_long.sol.bb.v11_median <- coords_wide.sol.bb.v11 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% rename(v11_sol_bb_Fe = value)

coords_long.sol.bb.v11_sd <- coords_wide.sol.bb.v11 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12))))  %>% rename(v11_sol_bb_Fe_sd = value)

all.v11.SOL <- full_join(coords_long.sol.du.v11_median, coords_long.sol.du.v11_sd) %>% 
  full_join(coords_long.sol.an.v11_median) %>% 
  full_join(coords_long.sol.an.v11_sd) %>% 
  full_join(coords_long.sol.bb.v11_median) %>% 
  full_join(coords_long.sol.bb.v11_sd)

# SOURCE APPORTIONED 
coal.ash.sims.appt <- full_join(all.v1.SOL, all.v3.SOL) %>% 
  full_join(all.v6.SOL) %>% 
  full_join(all.v7.SOL) %>% 
  full_join(all.v8.SOL) %>% 
  full_join(all.v9.SOL) %>% 
  full_join(all.v10.SOL) %>% 
  full_join(all.v11.SOL) %>%
  rename(Latitude = Model_Latitude_Values, 
         Longitude = Model_Longitude_Values)

coal.ash.sims.appt$Latitude <- as.numeric(coal.ash.sims.appt$Latitude)
coal.ash.sims.appt$Longitude <- as.numeric(coal.ash.sims.appt$Longitude)

coal.ash.sims.appt <- coal.ash.sims.appt %>% 
  mutate(region_4 = case_when(
    (Longitude >= 295 & Latitude <= -15 & Latitude > -48) | (Longitude < 30 & Latitude <= -15 & Latitude >= -48) ~ "SATL",
    (Longitude > 265 & Latitude > 29 & Latitude < 60) | (Longitude < 110 & Latitude > 29 & Latitude < 60) ~ "NATL",
    Longitude < 78 & Longitude >= 30 & Latitude <= 29 & Latitude > -48 ~ "AS",
    Longitude < 110 & Longitude >= 78 & Latitude <= 29 & Latitude > -48 ~ "BB",
    Longitude < 110 & Longitude >= 30 & Latitude <= 29 & Latitude > -48 ~ "INDO",
    Longitude < 150 & Longitude >= 110 & Latitude < 60 & Latitude > -15 ~ "SEAS",
    Longitude <= 265 & Longitude >= 150 & Latitude < 60 & Latitude > 29 ~ "NPAC",
    Latitude >= 60 & Longitude >= 0 ~ "ARCT" ,
    Longitude < 295 & Longitude > 110 & Latitude <= -15 & Latitude >= -48 ~ "AUSP",
    Longitude >= 0 & Latitude <= -48 & Latitude >= -90 ~ "SO",
    (Longitude >= 150 & Latitude > -15 & Latitude <= 29) | (Longitude < 30 & Latitude > -15 & Latitude <= 29) ~ "CPAO")) 

# Plots ---------------


coal.ash.sims.3 <- coal.ash.sims.appt %>% 
  dplyr::select(matches(c("_sol_","region")), -Obs_sol_Fe_sd, -v1_sol_Fe, -v3_sol_Fe, -v6_sol_Fe, -v7_sol_Fe, -v8_sol_Fe, -v9_sol_Fe) %>% 
  drop_na() %>% 
  pivot_longer(
  cols = matches("_sol_.*_Fe$"), 
  names_to = c("simulation", "aerosol_type"), 
  names_pattern = "^(.*)_sol_(.*)_Fe$", 
  values_to = "soluble_iron_conc")

obs_long <- coal.ash.sims.appt %>%
  dplyr::select(region_4, Obs_sol_Fe) %>% 
  rename(soluble_iron_conc = Obs_sol_Fe) %>% # Rename to match the long dataset
  mutate(
    simulation = "Obs",
    aerosol_type = "NA")

# Combine both datasets
coal.ash.sims.3 <- bind_rows(coal.ash.sims.3, obs_long) %>% 
  dplyr::select(-Obs_sol_Fe) %>% 
  drop_na() 

coal.ash.sims.summary.regions <- coal.ash.sims.3 %>% 
  group_by(region_4, simulation, aerosol_type) %>%
  summarise(mean_soluble_iron_conc = mean(soluble_iron_conc, na.rm = TRUE), .groups = "drop")%>%
  mutate(period = case_when(
    simulation == "Obs" ~ "Observation",
    simulation == "v1" ~ "Present Day",
    simulation == "v2" ~ "Present Day",
    simulation == "v3" ~ "Present Day",
    simulation == "v4" ~ "Present Day",
    simulation == "v6" ~ "Pre-Industrial",
    simulation == "v7" ~ "Pre-Industrial",
    simulation == "v8" ~ "Future SSP3-7.0",
    simulation == "v9" ~ "Future SSP3-7.0")) %>% 
  mutate(sol.settings = case_when(
    simulation == "v1" ~ "Base MIMI",
    simulation == "v6" ~ "Base MIMI",
    simulation == "v8" ~ "Base MIMI",
    simulation == "v3" ~ "Altered Solubility",
    simulation == "v7" ~ "Altered Solubility",
    simulation == "v9" ~ "Altered Solubility")) 

coal.ash.sims.summary.global <- coal.ash.sims.3 %>% 
  group_by(simulation, aerosol_type) %>%
  summarise(mean_soluble_iron_conc = mean(soluble_iron_conc, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = case_when(
    simulation == "Obs" ~ "Observation",
    simulation == "v1" ~ "Present Day",
    simulation == "v2" ~ "Present Day",
    simulation == "v3" ~ "Present Day",
    simulation == "v4" ~ "Present Day",
    simulation == "v6" ~ "Pre-Industrial",
    simulation == "v7" ~ "Pre-Industrial",
    simulation == "v8" ~ "Future SSP3-7.0",
    simulation == "v9" ~ "Future SSP3-7.0")) %>% 
  mutate(sol.settings = case_when(
    simulation == "v1" ~ "Base MIMI",
    simulation == "v6" ~ "Base MIMI",
    simulation == "v8" ~ "Base MIMI",
    simulation == "v3" ~ "Altered Solubility",
    simulation == "v7" ~ "Altered Solubility",
    simulation == "v9" ~ "Altered Solubility")) 

library(ggpattern)
library(readxl)
library(forcats) 

coal.ash.sims.3 <- read_excel("C:/Users/heplaas/OneDrive - North Carolina State University/Collaborations/Coal Fly Ash/data/FeDepositionBudgets.xlsx", sheet = "all_versions_final", na = c("NA",""))

coal.ash.sims.3.filtered <- coal.ash.sims.3 %>% 
  filter(Variable != "FESOLDEP_mean" , Variable !="FETOTDEP_mean") %>% 
  mutate(settings_combo = paste(Scenario, Simulation, sep = "_")) 

simulation.color.palette <- c("PD_V1"   = "#a8ffa8",
                              "PD_V2" = "#04ff00",
                              "PD_V3" = "#0f7100",
                              "PD_V4" = "#0c3b0c" , 
                              "PI_V1" = "#1A1A1A80", 
                              "PI_V3" = "#2e2e2e", 
                              "SSP370mid_V1" = "#cc5a0080", 
                              "SSP370mid_V3" = "#cc5a00",
                              "SSP370end_V1" = "#a93b0080",
                              "SSP370end_V3" = "#a93b00")

period.color.palette <- c("PI"= "#000000",
                          "PD"= "#0a4c00",
                          "SSP370mid"= "#ab4c00",
                          "SSP370end"= "#832e00")

aerosol.source.patterns <- c("FEDUSOLDEP_mean" = "stripe",
                             "FEDUTOTDEP_mean" = "stripe",
                             "FEANSOLDEP_mean"   = "circle",
                             "FEANTOTDEP_mean"   = "circle",
                             "FEBBSOLDEP_mean"   = NA,
                             "FEBBTOTDEP_mean"   = NA)


coal.ash.sims.3.soluble <- coal.ash.sims.3.filtered %>% 
  filter(!grepl("TOT", Variable)) %>%
  mutate(settings_combo = fct_relevel(settings_combo, "PI_V1", "PI_V3", "PD_V1", "PD_V2", "PD_V3", "PD_V4", "SSP370mid_V1", "SSP370mid_V3", "SSP370end_V1", "SSP370end_V3"))

coal.ash.sims.3.total <- coal.ash.sims.3.filtered %>% 
  filter(!grepl("SOL", Variable)) %>%
  mutate(settings_combo = fct_relevel(settings_combo, "PI_V1", "PI_V3", "PD_V1", "PD_V2", "PD_V3", "PD_V4", "SSP370mid_V1", "SSP370mid_V3", "SSP370end_V1", "SSP370end_V3"))

ggplot(coal.ash.sims.3.total, aes(x = settings_combo, y = Total_Budget, 
                 fill = settings_combo, color = Scenario, 
                 pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,38))

Global_sol <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = Total_Budget, 
                                  fill = settings_combo, color = Scenario, 
                                  pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.6))

SO <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = SO_Budget, 
                                            fill = settings_combo, color = Scenario, 
                                            pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.01))

SEAS <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = SEAS_Budget, 
                                            fill = settings_combo, color = Scenario, 
                                            pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.15))

BB <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = BB_Budget, 
                                          fill = settings_combo, color = Scenario, 
                                          pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.15))

AUSP <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = AUSP_Budget, 
                                            fill = settings_combo, color = Scenario, 
                                            pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.6))

NATL <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = NATL_Budget, 
                                            fill = settings_combo, color = Scenario, 
                                            pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.6))

NPAC <- ggplot(coal.ash.sims.3.soluble, aes(x = settings_combo, y = NPAC_Budget, 
                                            fill = settings_combo, color = Scenario, 
                                            pattern = Variable, pattern_fill = Scenario)) +  # Map pattern fill
  geom_bar_pattern(stat = "identity", position = "stack", linewidth = 1.2, 
                   pattern_density = 0.3) +  # Outline color of the pattern  
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +  
  scale_pattern_manual(values = aerosol.source.patterns) +  
  scale_pattern_fill_manual(values = period.color.palette) +  # Map pattern fill
  theme_bw() + 
  ylim(c(0,0.05))

SATL + SEAS + BB + NPAC + NATL + AUSP


solsettings.color.palette <- c("Altered Solubility"= "#ff0000",
                               "Base MIMI" = "#000000")

coal.ash.sims.summary.global %>% 
  ggplot(aes(x = simulation, y = mean_soluble_iron_conc, fill = simulation, color = period, 
            pattern = aerosol_type, 
            #pattern_fill = simulation, pattern_color = period
             )) +
  geom_bar_pattern(stat = "identity", position = "stack") + 
  scale_fill_manual(values = simulation.color.palette) +
  scale_color_manual(values = period.color.palette) +
  scale_pattern_manual(values = aerosol.source.patterns) #+ 
  scale_pattern_fill_manual(values = simulation.color.palette) + 
  scale_pattern_color_manual(values = period.color.palette) 
  



# Boxplots comparing FU, PI, and PD simulations -------------------------------------------------------
  coal.ash.sims.2 <- coal.ash.sims %>% 
    dplyr::select(matches(c("_sol_","region")), -Obs_sol_Fe_sd) %>% 
    drop_na() %>% 
    pivot_longer(
      cols = matches("_sol_Fe$"), 
      names_to = "simulation",        
      values_to = "Sol_Fe",       
      names_pattern = "(.*)_sol_Fe") %>% 
    filter(simulation != "v5") %>%
    mutate(period = case_when(
      simulation == "Obs" ~ "Observation",
      simulation == "v1" ~ "Present Day",
      simulation == "v2" ~ "Present Day",
      simulation == "v3" ~ "Present Day",
      simulation == "v4" ~ "Present Day",
      simulation == "v6" ~ "Pre-Industrial",
      simulation == "v7" ~ "Pre-Industrial",
      simulation == "v8" ~ "Future SSP3-7.0",
      simulation == "v9" ~ "Future SSP3-7.0")) %>% 
    mutate(sol.settings = case_when(
      simulation == "v1" ~ "Base MIMI",
      simulation == "v6" ~ "Base MIMI",
      simulation == "v8" ~ "Base MIMI",
      simulation == "v3" ~ "Altered Solubility",
      simulation == "v7" ~ "Altered Solubility",
      simulation == "v9" ~ "Altered Solubility")) 
  
  simulation.color.palette <- c("Obs" = "#001eb680",
                                "v1"   = "#a8ffa880",
                                "v2" = "#04ff0080",
                                "v3" = "#029a0080",
                                "v4" = "#0c3b0c80" , 
                                "v6" = "#96969680", 
                                "v7" = "#1A1A1A80", 
                                "v8" = "#d5492780", 
                                "v9" = "#551d1080")
  
  period.color.palette <- c("Pre-Industrial"= "#000000",
                            "Present Day"= "#0f7100",
                            "Future SSP3-7.0"= "#cc5a00")
  
  aerosol.source.patterns <- c("du" = "stripe",
                               "an"   = "circle",
                               "bb" = "crosshatch",
                               "Obs" = "placeholder")
  
  solsettings.color.palette <- c("Altered Solubility"= "#ff0000",
                                 "Base MIMI" = "#000000")
  
  library(ggpubr)
  
  p <- ggplot(coal.ash.sims.2, aes(x = simulation, y = Sol_Fe, fill = simulation, color = period)) +
    geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
    facet_wrap(~ region_4_combined) +
    # facet_wrap(~ region_4) +
    labs(x = "Model Simulation", y = "Soluble Iron (ng m-3)") +
    theme_bw() +
    scale_fill_manual(values = simulation.color.palette) +
    scale_color_manual(values = period.color.palette) #+
  # coord_cartesian(ylim = c(0, 5)) 
  
  p + stat_compare_means(comparisons = list(
    c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), 
    label = "p.signif", 
    method = "wilcox",
    hide.ns = T, 
    tip.length = 0,
    textsize = 3,
    bracket.size = 1,
    step.increase = 0.15, 
    hjust = -.5)
  

# boxplots to show differences between modeled and simulated Fe solubilities, grouped by region --------------
# Soluble Iron concentrations
library(ggpubr)
coal.ash.sims.boxplot <- coal.ash.sims %>% 
  dplyr::select(region_4, ends_with("sol_Fe")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("sol_Fe"), names_to = c("data_source", ".value"), names_sep = "_") %>%
  rename(Soluble_Fe = sol) # %>% 
# filter(data_source != c("v6", "v7", "v8"))

ggplot(coal.ash.sims.boxplot, aes(x = data_source, y = Soluble_Fe, fill = data_source)) +
  geom_boxplot(coef = 2.5) + #default is 1.5, switching to 2.5, IQR2 obs ~ IQR1 for models
  facet_wrap(~ region_4) +
  labs(x = "Data Source", y = "Soluble Iron (ng m-3)", title = "Soluble Iron Concentrations by Region") +
  theme_bw() +
  scale_fill_brewer(palette = "Spectral") +
  stat_compare_means(comparisons = list(c("Obs", "v1"), c("Obs", "v2"), c("Obs", "v3"), c("Obs", "v4")), label = "p.signif", method = "wilcox",hide.ns = T,tip.length = 0,textsize = 3,bracket.size = 1,step.increase = 0.15, vjust = .25) +
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




