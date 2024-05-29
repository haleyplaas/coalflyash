setwd("Q:\\My Drive\\Code Repositories\\R\\coalflyash")
rm(list = ls())

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc)

sim.1 <- "Q:\\My Drive\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-QFED_SRF_2018.nc"
model.data.QFED <- nc_open(sim.1)
sim.2 <- "Q:\\My Drive\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI-FINN_SRF_2018.nc"
model.data.FINN <- nc_open(sim.2)

# QFED 2018
variable.QFED <- ncvar_get(model.data.QFED, "FETOTSRF")
variable.sol.QFED <- ncvar_get(model.data.QFED, "FESOLSRF")

# FINN 2018
variable.FINN <- ncvar_get(model.data.FINN, "FETOTSRF")
variable.sol.FINN <- ncvar_get(model.data.FINN, "FESOLSRF")

# For QFED and FINN (or any datasets that have daily values)
FETOTSRF.QFED <- variable.QFED[ , , ] 
FESOLSRF.QFED <- variable.sol.QFED[ , , ]

FETOTSRF.FINN <- variable.FINN[ , , ] 
FESOLSRF.FINN <- variable.sol.FINN[ , , ]

#QFED
annual_average.QFED <- apply(FETOTSRF.QFED, c(1, 2), mean, na.rm = TRUE)
annual_average.sol.QFED <- apply(FESOLSRF.QFED, c(1, 2), mean, na.rm = TRUE) 
annual_median.QFED <- apply(FETOTSRF.QFED, c(1, 2), median, na.rm = TRUE)
annual_median.sol.QFED <- apply(FESOLSRF.QFED, c(1, 2), median, na.rm = TRUE)

#FINN
annual_average.FINN <- apply(FETOTSRF.FINN, c(1, 2), mean, na.rm = TRUE)  
annual_average.sol.FINN <- apply(FESOLSRF.FINN, c(1, 2), mean, na.rm = TRUE) 
annual_median.FINN <- apply(FETOTSRF.FINN, c(1, 2), median, na.rm = TRUE)
annual_median.sol.FINN <- apply(FESOLSRF.FINN, c(1, 2), median, na.rm = TRUE)

nc_close(model.data.QFED)
nc_close(model.data.FINN)

# Create a dataframe with latitude, longitude, and annual AVERAGE # QFED --------------------------
coords.QFED <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                          longitude = rep(all.long.options, length(all.lat.options)),
                          annual_average.QFED = as.vector(annual_average.QFED)) 
coords_wide.QFED <- pivot_wider(coords.QFED, names_from = longitude, values_from = annual_average.QFED)
coords_wide.QFED <- spread(coords.QFED, key = longitude, value = annual_average.QFED) 
rownames(coords_wide.QFED) <- all.lat.options 
coords_wide.QFED <- coords_wide.QFED[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.QFED <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_average.sol.QFED = as.vector(annual_average.sol.QFED))
coords_wide.sol.QFED <- pivot_wider(coords.sol.QFED, names_from = longitude, values_from = annual_average.sol.QFED)
coords_wide.sol.QFED <- spread(coords.sol.QFED, key = longitude, value = annual_average.sol.QFED) 
rownames(coords_wide.sol.QFED) <- all.lat.options 
coords_wide.sol.QFED <- coords_wide.sol.QFED[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# Create a dataframe with latitude, longitude, and annual MEDIAN
coords.med.QFED <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_median.QFED = as.vector(annual_median.QFED)) 
coords_wide.med.QFED <- pivot_wider(coords.med.QFED, names_from = longitude, values_from = annual_median.QFED)
coords_wide.med.QFED <- spread(coords.med.QFED, key = longitude, value = annual_median.QFED) 
rownames(coords_wide.med.QFED) <- all.lat.options 
coords_wide.med.QFED <- coords_wide.med.QFED[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.med.QFED <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                  longitude = rep(all.long.options, length(all.lat.options)),
                                  annual_median.sol.QFED = as.vector(annual_median.sol.QFED))
coords_wide.sol.med.QFED <- pivot_wider(coords.sol.med.QFED, names_from = longitude, values_from = annual_median.sol.QFED)
coords_wide.sol.med.QFED <- spread(coords.sol.med.QFED, key = longitude, value = annual_median.sol.QFED) 
rownames(coords_wide.sol.med.QFED) <- all.lat.options 
coords_wide.sol.med.QFED <- coords_wide.sol.med.QFED[,-1] 

# Create a dataframe with latitude, longitude, and annual AVERAGE # FINN --------------------------
coords.FINN <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                          longitude = rep(all.long.options, length(all.lat.options)),
                          annual_average.FINN = as.vector(annual_average.FINN)) 
coords_wide.FINN <- pivot_wider(coords.FINN, names_from = longitude, values_from = annual_average.FINN)
coords_wide.FINN <- spread(coords.FINN, key = longitude, value = annual_average.FINN) 
rownames(coords_wide.FINN) <- all.lat.options 
coords_wide.FINN <- coords_wide.FINN[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.FINN <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_average.sol.FINN = as.vector(annual_average.sol.FINN))
coords_wide.sol.FINN <- pivot_wider(coords.sol.FINN, names_from = longitude, values_from = annual_average.sol.FINN)
coords_wide.sol.FINN <- spread(coords.sol.FINN, key = longitude, value = annual_average.sol.FINN) 
rownames(coords_wide.sol.FINN) <- all.lat.options 
coords_wide.sol.FINN <- coords_wide.sol.FINN[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# Create a dataframe with latitude, longitude, and annual MEDIAN
coords.med.FINN <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                              longitude = rep(all.long.options, length(all.lat.options)),
                              annual_median.FINN = as.vector(annual_median.FINN)) 
coords_wide.med.FINN <- pivot_wider(coords.med.FINN, names_from = longitude, values_from = annual_median.FINN)
coords_wide.med.FINN <- spread(coords.med.FINN, key = longitude, value = annual_median.FINN) 
rownames(coords_wide.med.FINN) <- all.lat.options 
coords_wide.med.FINN <- coords_wide.med.FINN[,-1] #need to check annual value with the manual script -- worked for ncl_a1 and FETOTSRF 
# and specifying for soluble iron (averaged)
coords.sol.med.FINN <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                                  longitude = rep(all.long.options, length(all.lat.options)),
                                  annual_median.sol.FINN = as.vector(annual_median.sol.FINN))
coords_wide.sol.med.FINN <- pivot_wider(coords.sol.med.FINN, names_from = longitude, values_from = annual_median.sol.FINN)
coords_wide.sol.med.FINN <- spread(coords.sol.med.FINN, key = longitude, value = annual_median.sol.FINN) 
rownames(coords_wide.sol.med.FINN) <- all.lat.options 
coords_wide.sol.med.FINN <- coords_wide.sol.med.FINN[,-1] 

# For loop to compile observational data with models values -- for QFED  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
QFED_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR QFED 
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
    extracted_value <- coords_wide.med.QFED[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  QFED_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$QFED_values <- QFED_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
QFED_values.sol <- numeric(nrow(obs.data.mod.res.n1))
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
    extracted_value <- coords_wide.sol.med.QFED[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  QFED_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$QFED_values.sol <- QFED_values.sol

# creating new dataframe just with the important information 
# obs.mod.comparison.n1 <- obs.data.mod.res.n1 %>% dplyr::select(Latitude, Longitude, `Fe (ng m–3)`,`Fe solubility`,`labile Fe (ng m–3)`, mod_values, mod_values.sol) %>% rename(Obs_Fe = `Fe (ng m–3)`, Mod_Fe = `mod_values`, Obs_sol_Fe = `labile Fe (ng m–3)`, Mod_sol_Fe = `mod_values.sol`, Obs_solubility = `Fe solubility`) %>% mutate(Mod_solubility = Mod_sol_Fe / Mod_Fe) # use this when not compiling observations by grid
obs.mod.comparison.QFED <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, QFED_values, QFED_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, QFED_Fe = `QFED_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, QFED_sol_Fe = `QFED_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(QFED_solubility = QFED_sol_Fe / QFED_Fe)

# For loop to compile obs with models values -- for FINN  ---------------------------------------------------- 
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values.n1 <- obs.data.mod.res.n1$Model_Latitude_Values[[1]]
model_longitude_values.n1 <- obs.data.mod.res.n1$Model_Longitude_Values[[1]]

# Initialize an empty vector to store the mod values for each row
FINN_values <- numeric(nrow(obs.data.mod.res.n1))
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values)
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values)

# Loop over each row of obs_data FOR FINN 
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
    extracted_value <- coords_wide.med.FINN[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  FINN_values[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$FINN_values <- FINN_values

# Initialize an empty vector to store the mod values for each row (NOW FOR SOLUBILITY)
FINN_values.sol <- numeric(nrow(obs.data.mod.res.n1))
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
    extracted_value <- coords_wide.sol.med.FINN[as.character(lat), as.character(lon)]
    
    # Store the extracted value
    extracted_values[i] <- extracted_value
  }
  
  # convert each value from kg/kg to ng/m3 for direct comparison to observational data
  extracted_values <- extracted_values*((100000/(287*273.15))*10^12)
  
  # Store the extracted values for the current row in the mod_values vector
  FINN_values.sol[row_index] <- mean(extracted_values, na.rm = TRUE)  # Use mean to calculate average
  
}

# Add mod_values as a new column to obs_data
obs.data.mod.res.n1$FINN_values.sol<- FINN_values.sol

# creating new dataframe just with the important information 
# obs.mod.comparison.n1 <- obs.data.mod.res.n1 %>% dplyr::select(Latitude, Longitude, `Fe (ng m–3)`,`Fe solubility`,`labile Fe (ng m–3)`, mod_values, mod_values.sol) %>% rename(Obs_Fe = `Fe (ng m–3)`, Mod_Fe = `mod_values`, Obs_sol_Fe = `labile Fe (ng m–3)`, Mod_sol_Fe = `mod_values.sol`, Obs_solubility = `Fe solubility`) %>% mutate(Mod_solubility = Mod_sol_Fe / Mod_Fe) # use this when not compiling observations by grid
obs.mod.comparison.FINN <- obs.data.mod.res.n1 %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, `Fe (ng m–3)_median`,`Fe solubility_median`,`labile Fe (ng m–3)_median`, FINN_values, FINN_values.sol) %>% rename(Latitude = Model_Latitude_Values, Longitude = Model_Longitude_Values, Obs_Fe = `Fe (ng m–3)_median`, FINN_Fe = `FINN_values`, Obs_sol_Fe = `labile Fe (ng m–3)_median`, FINN_sol_Fe = `FINN_values.sol`, Obs_solubility = `Fe solubility_median`) %>% mutate(FINN_solubility = FINN_sol_Fe / FINN_Fe)


# merging QFED and FINN dataframes for easier comparisons (eventually add medians AND means) ----------------------
iron.dataset.sims <- cbind(obs.mod.comparison.QFED, obs.mod.comparison.FINN)
iron.dataset.sims  <- iron.dataset.sims[, !duplicated(colnames(iron.dataset.sims))]



