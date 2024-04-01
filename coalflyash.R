setwd("Q:\\My Drive\\Code Repositories\\R\\coalflyash")
rm(list = ls())

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl);library(ncdf4);library(raster);library(lubridate) 
library(RNetCDF)

#install.packages("")

# Read in observational data (excel file) with specified parameters -----------------------
data.file <- "Q:\\My Drive\\Collaborations\\Coal Fly Ash\\data\\FeObs_Hamilton2022.xlsx"

obs.data <- read_excel(data.file, sheet = "FeObs_Hamilton2021",
                 col_names = TRUE,
                 na = c("-99", "-9999", "-99.0","-0.99"))

# function convert any longitude encoded as deg W to 360 E 
convert_longitude <- function(longitude) {
  if (longitude < 0) {
    return(round(360 - abs(longitude), 1))
  } else {
    return(round(longitude, 1))
  }
}

# Convert Longitude column
obs.data$Longitude <- sapply(obs.data$Longitude, convert_longitude)


# Read in the simulations data (netcdf files) ----------------------------------------------
simulation.x <- "Q:\\My Drive\\Collaborations\\Fe-SSA\\FeSSA nc files\\FeSSA_UMich2.cam.h1.2018-Oct-Dec.nc"
model.data.x <- nc_open(simulation.x) #opening the netcdf file as a list 

# creating vectors of all possible longitude, latitude, and timepoint options
all.long.options <- ncvar_get(model.data.x, "lon")
all.lat.options <- ncvar_get(model.data.x, "lat")
all.lev.options <- ncvar_get(model.data.x, "lev")
all.time.options <- ncvar_get(model.data.x, "time")


# assigning model values to average (spatially) for each observational data point -------------------
# at the grid-box resolution (1 grid box, n=4)
find_model_latitude <- function(df, all_lat_options) {
  df <- df %>%
    mutate(Model_Latitude_Values = sapply(Latitude, function(lat) {
      matching_indices <- which(abs(all_lat_options - lat) <= 0.942408)
      if (length(matching_indices) > 0) {
        paste(all_lat_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  
  return(df)
}
obs.data.1 <- find_model_latitude(obs.data, all.lat.options)
find_model_longitude <- function(df, all_long_options) {
  df <- df %>%
    mutate(Model_Longitude_Values = sapply(Longitude, function(long) {
      matching_indices <- which(abs(all_long_options - long) <= 1.25)
      if (length(matching_indices) > 0) {
        paste(all_long_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  
  return(df)
}
obs.data.mod.res <- find_model_longitude(obs.data.1, all.long.options)

# extracting latitudes and longitudes that will correspond with observational data
Obs.Lats <- unlist(strsplit(obs.data.mod.res$Model_Latitude_Values, ", "))
Obs.Long <- unlist(strsplit(obs.data.mod.res$Model_Longitude_Values, ", "))

# Remove duplicates and sort the vectors
Obs.Lats <- sort(unique(Obs.Lats))
Obs.Long <- sort(unique(Obs.Long))

# at 1/3 the model resolution (9 grid boxes, n =16)
find_model_latitude <- function(df, all_lat_options) {
  df <- df %>%
    mutate(Model_Latitude_Values = sapply(Latitude, function(lat) {
      matching_indices <- which(abs(all_lat_options - lat) <= 1.884816)
      if (length(matching_indices) > 0) {
        paste(all_lat_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  
  return(df)
}
obs.data.1 <- find_model_latitude(obs.data, all.lat.options)
find_model_longitude <- function(df, all_long_options) {
  df <- df %>%
    mutate(Model_Longitude_Values = sapply(Longitude, function(long) {
      matching_indices <- which(abs(all_long_options - long) <= 2.5)
      if (length(matching_indices) > 0) {
        paste(all_long_options[matching_indices], collapse = ", ")
      } else {
        NA
      }
    }))
  
  return(df)
}
obs.data.third.mod.res <- find_model_longitude(obs.data.1, all.long.options)

# extracting latitudes and longitudes that will correspond with observational data
Obs.Lats.third <- unlist(strsplit(obs.data.third.mod.res$Model_Latitude_Values, ", "))
Obs.Long.third <- unlist(strsplit(obs.data.third.mod.res$Model_Longitude_Values, ", "))

# Remove duplicates and sort the vectors
Obs.Lats.third <- sort(unique(Obs.Lats.third))
Obs.Long.third <- sort(unique(Obs.Long.third))


# Convert whole array to dataframe and calculate averages at annual resolution (whatever resolution you specify) this passed cross examination in Panoply and using single-point extractions averaged over time period specified ----------- 
# Extract the necessary variable
variable <- ncvar_get(model.data.x, "ncl_a1")
dim(variable)

# Filter the data to consider only the first level (surface altitude)
ncl_a1 <- variable[, , 1, ]  # Assuming dimensions are (longitude, latitude, level, time), 1 corresponds to surface

# Calculate the annual average
annual_average <- apply(ncl_a1, c(1, 2), mean, na.rm = TRUE)  # Calculate mean over time dimension

# Combine latitude and longitude data
coords <- expand.grid(latitude = all.lat.options, longitude = all.long.options)
coords$annual_average <- as.vector(annual_average)

# Or create a dataframe with latitude, longitude, and annual average
coords <- data.frame(latitude = rep(all.lat.options, each = length(all.long.options)),
                     longitude = rep(all.long.options, length(all.lat.options)),
                      annual_average = as.vector(annual_average))

# Pivot the dataframe wider
coords_wide <- pivot_wider(coords, names_from = all.long.options, values_from = annual_average)

# Set latitude as row names
coords_wide <- spread(coords, key = longitude, value = annual_average) #note that the final first column is filled in with the latitude values 
rownames(coords_wide) <- all.lat.options 
coords_wide <- coords_wide[,-1] #need to check annual value with the manual script -- let's try -90 lat 0.0 long


# this function works to extract values for a single location on a single date, cross examined using Panopoly array -- this stopped working on 4/1/2024 per EXTRACT being cross listed as another command in tidyr- fixed by specifying raster:: ----
names(model.data.x$var)

# check with Douglas on the date delay issue 
# change the simulation (case) and variable assignment here
library(raster)
sim.var.1 <- brick(simulation.x, var='ncl_a1', level = 1) 
sim.var.2 <- brick(simulation.x, var='ncl_a2', level = 1)

# Function that extracts the level 1 value a specified long-lat pair
extract_geocoord_value <- function(longitude, latitude, sim.var) {
  # Extract the value at the specified longitude and latitude
  value <- raster::extract(sim.var, cbind(longitude, latitude))
  
  # Convert to dataframe
  value_df <- as.data.frame(value)
  
  # Pivot the dataframe
  value_by_date <- pivot_longer(value_df, 
                                cols = everything(), 
                                names_to = "Date", 
                                names_prefix = "X", 
                                values_to = "Value") %>%
    mutate(Date = as.Date(Date, format = "%Y.%m.%d") - days(1))
  
  return(value_by_date)
}

NaCl_accum <- extract_geocoord_value(0, -90, sim.var.1) # checking in Panopoly for accuracy -- passed
mean_value <- mean(NaCl_accum$Value, na.rm = TRUE) #testing accuracy of array -- it passed for 0E -90N
NaCl_accum <- extract_geocoord_value(123.75, 87.17278, sim.var.1) # checking in Panopoly for accuracy -- passed
mean_value <- mean(NaCl_accum$Value, na.rm = TRUE) #testing accuracy of array -- it passed for 123.75E 87.17E
NaCl_ait <- extract_geocoord_value(254, -9.9, sim.var.2) # checking in Panopoly for accuracy -- passed, didn't work for U10, due to issue with level? so careful to only use this when looking at surface variables OR make sure level is changed accordingly














# For loop to extract the corresponding model values for each specific observation, then averaged, and added to final dataframe
# Extract latitude and longitude values from the first row of obs_data
model_latitude_values <- obs.data.mod.res$Model_Latitude_Values[[1]]
model_longitude_values <- obs.data.mod.res$Model_Longitude_Values[[1]]

# Extract values from coords_wide based on latitude and longitude values
# Initialize an empty vector to store the mod averages for each row
mod_averages <- numeric(nrow(obs.data.mod.res))

# Loop over each row of obs_data
for (row_index in seq_len(nrow(obs.data.mod.res))) {
  # Extract latitude and longitude values from the current row of obs_data
  model_latitude_values <- obs.data.mod.res$Model_Latitude_Values[[row_index]]
  model_longitude_values <- obs.data.mod.res$Model_Longitude_Values[[row_index]]
  
  # Extract values from coords_wide based on latitude and longitude values
  extracted_values <- numeric(length(model_latitude_values))
  for (i in seq_along(model_latitude_values)) {
    lat <- unlist(strsplit(model_latitude_values[i], ", "))  # Split lat string into separate values
    lon <- unlist(strsplit(model_longitude_values[i], ", "))  # Split lon string into separate values
    
    # Iterate over each latitude value (in case there are multiple)
    for (j in seq_along(lat)) {
      # Iterate over each longitude value (in case there are multiple)
      for (k in seq_along(lon)) {
        # Find row and column names in coords_wide that match the current latitude and longitude
        lat_row_name <- toString(lat[j])
        lon_col_name <- toString(lon[k])
        
        # Extract the value from coords_wide based on the row and column name
        extracted_value <- coords_wide[lat_row_name, lon_col_name]
        
        # Assign the extracted value to the extracted_values vector
        extracted_values <- c(extracted_values, extracted_value)
      }
    }
  }
  
  # Remove the first element (initialized as 0) from extracted_values
  extracted_values <- extracted_values[-1]
  
  # Calculate the average of the extracted values
  mod_average <- mean(extracted_values, na.rm = TRUE)
  
  # Store the mod_average for the current row in the mod_averages vector
  mod_averages[row_index] <- mod_average
}

# Add mod_averages as a new column to obs_data
obs.data.mod.res$mod_average <- mod_averages

# creating new dataframe just with the important information
obs.mod.comparison <- obs.data.mod.res %>% select(Latitude, Longitude, `Fe (ng m–3)`,`Fe solubility`,`labile Fe (ng m–3)`, mod_average)

# next steps will be to update everything specific to IRON variables of interest (I assume FETOTSRF and FESOLSRF -- try with old MIMI files first the QFED ones)



















