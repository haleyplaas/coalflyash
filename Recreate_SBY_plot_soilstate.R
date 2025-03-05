setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\coalflyash")
rm(list = ls())

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(patchwork);library(readxl);library(raster);library(lubridate);library(RNetCDF);library(ggpmisc);library(purrr);library(ncdf4);library(dplyr);library(tidyr);library(tibble)

# ------------------------------ READING IN AND CLEANING DATA -------------------------------------
# Read in observational data (excel file) with specified parameters -----------------------
data.file <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\24.10.16_FeObs_Hamilton2022_updated2024_Tang.xlsx"

obs.data <- read_excel(data.file, sheet = "Tang_approved_comparisons",
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
# when multiple timepoints still exist -- find averages and standard deviations over time
generate_median_values_from_netcdf <- function(file_directory, var1_name, var2_name) {
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
  
  # Calculate mean values for the specified variables
  mean_var1 <- apply(variable1, c(1, 2), median, na.rm = TRUE)
  mean_var2 <- apply(variable2, c(1, 2), median, na.rm = TRUE)
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
  
  # Calculate mean values for the specified variables
  mean_var1 <- apply(variable1, c(1, 2), mean, na.rm = TRUE)
  mean_var2 <- apply(variable2, c(1, 2), mean, na.rm = TRUE)
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


# PD SIMULATIONS -----------------------------------------------------
# SOIL STATE
#result <- generate_mean_values_from_netcdf("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Collaborations\\Coal Fly Ash\\data\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38.cam.h2.2009-2011_PD.nc", "FETOTSRF", "FESOLSRF")
# NO SOIL STATE
result <- generate_median_values_from_netcdf("D:\\CoalFlyAsh\\no_soil_state_firex1_runs\\no_soil_state_an+du_cheyenne_bb_derecho.h2_v1.nc", "FETOTSRF", "FESOLSRF")
coords_wide.med.v1 <- result[[1]]
coords_wide.sol.med.v1 <- result[[2]]
coords_wide.med.v1_sd <- result[[3]]
coords_wide.sol.med.v1_sd <- result[[4]]

# get dimensions of netcdf files to store for later ----------------------------------------
dim.file <- "D:\\CoalFlyAsh\\CAM6-MIMI_2010CLIMO_INDCOAL0.2-RESICOAL0.2-WOOD10-OIL38_2009-2011.h2.BBx1_ANx2_DUx2.nc"
nc_data <- nc_open(dim.file)
all_lon_options <- ncvar_get(nc_data, "lon")
all_lat_options <- ncvar_get(nc_data, "lat")
all_time_options <- ncvar_get(nc_data, "time")
nc_close(nc_data)

# assigning variable for changes in data resolution (soft-coding for quicker long-term plotting/ analyses)
d.lat <- abs(all_lat_options[2] - all_lat_options[1])  # Latitude grid spacing in degrees
d.lon <- abs(all_lon_options[2] - all_lon_options[1])  # Longitude grid spacing in degrees
d.time <- abs(all_time_options[2] - all_time_options[1])

# INDIVIDUAL COMPARISONS OBS TO MOD -------------------------
obs.data.ind <- obs.data %>%
  mutate(Model_Latitude_Values = map_dbl(Latitude, ~ all_lat_options[which.min(abs(all_lat_options - .x))]))
obs.data.mod.res <- obs.data.ind %>%
  mutate(Model_Longitude_Values = map_dbl(Longitude, ~ all_lon_options[which.min(abs(all_lon_options - .x))]))

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

# combine observational with modeled data ----------------------------------
obs.data.mod.res.n1$Model_Latitude_Values <- as.character(obs.data.mod.res.n1$Model_Latitude_Values) 
obs.data.mod.res.n1$Model_Longitude_Values <- as.character(obs.data.mod.res.n1$Model_Longitude_Values) 

# converting Lat and Lon to character variables
obs.data.mod.res.n1 <- obs.data.mod.res.n1 %>%
  mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values))

coords_long.med.v1_median <- coords_wide.med.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_Fe = value)

coords_long.med.v1_sd <- coords_wide.med.v1_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_Fe_sd = value)

coords_long.med.v1 <- full_join(coords_long.med.v1_median, coords_long.med.v1_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v1_Fe, v1_Fe_sd, everything())

coords_long.sol.med.v1_median <- coords_wide.sol.med.v1 %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_sol_Fe = value)

coords_long.sol.med.v1_sd <- coords_wide.sol.med.v1_sd %>% rownames_to_column(var = "Model_Latitude_Values") %>%  pivot_longer(cols = -Model_Latitude_Values, names_to = "Model_Longitude_Values", values_to = "value") %>% mutate(value = (value*((100000/(287*273.15))*(10^12)))) %>% mutate(Model_Latitude_Values = as.numeric(Model_Latitude_Values), Model_Longitude_Values = as.numeric(Model_Longitude_Values)) %>% right_join(obs.data.mod.res.n1, by = c("Model_Latitude_Values", "Model_Longitude_Values")) %>% rename(v1_sol_Fe_sd = value)

coords_long.sol.med.v1 <- full_join(coords_long.sol.med.v1_median, coords_long.sol.med.v1_sd) %>% dplyr::select(Model_Latitude_Values, Model_Longitude_Values, v1_sol_Fe, v1_sol_Fe_sd, everything())

# COMPILING  MODELED RESULTS -----
all.TOTFe <- coords_long.med.v1

all.SOLFe <- coords_long.sol.med.v1
                       
# COMPILING ALL SOLUBLE IRON DATA ---------
all.SOLFe <- coords_long.sol.med.v1

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
  #  mutate(v1_Fe = v1_Fe*3.8, v1_sol_Fe = v1_sol_Fe*3.8) %>% # AOD correction for dust
    mutate(v1_solubility = v1_sol_Fe / v1_Fe,
           v1_solubility_sd = v1_sol_Fe_sd / v1_Fe_sd)

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

India.palette <- c("ARCT" = "#1B9E7790",
                   "SO"   = "#1F78B490" ,
                   "NATL" = "#7570B390",
                   "SATL" = "#E7298A90",
                   "CPAO" = "#66A61E90" , 
                   "NPAC" = "#E6AB0290" , 
                   "ENPAC" = "#c4a13b90" ,
                   "WNPAC" = "#ffd14d90" ,
                   "SEAS" = "#8d6c2f90", 
                   "AUSP" = "#66666690", 
                   "BB" = "#D95F0290", 
                   "AS" = "#2d519290", 
                   "INDO" = "#FF7F0090")

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

# Create a new column that combines AS and BB into INDO and E and W NPAC into NPAC
coal.ash.sims$region_4_combined <- coal.ash.sims$region_4
coal.ash.sims$region_4_combined[coal.ash.sims$region_4 %in% c("AS", "BB")] <- "INDO"
coal.ash.sims$region_4_combined[coal.ash.sims$region_4 %in% c("ENPAC", "WNPAC")] <- "NPAC"

# ------------------------------------- VISUALIZATIONS --------------------------------------------
# Linear Regression plots comparing obs and modeled values independent of region on a log scale 
# making single points visualized in regression for easier interpretation ------------------------------
region_count <- coal.ash.sims %>% group_by(region_4) %>% summarize(count = n())

# ADDING DETAILS FOR INDO AND NPAC COMBINED
coal.ash.sims.1 <- coal.ash.sims %>% 
  mutate(across(-c(region_4, region_4_combined), as.numeric)) %>%
  dplyr::select(-Latitude, -Longitude, -region_4_combined) %>%
  group_by(region_4) %>%
  summarize(across(everything(), median, na.rm = TRUE)) %>% # did I change this to median?
  ungroup() %>% 
  full_join(region_count) %>% 
  mutate(count = as.numeric(count)) %>% drop_na()

indo_row <- coal.ash.sims.1 %>%
  filter(region_4 %in% c("BB", "AS")) %>%
  dplyr::select(-matches(c("count"))) %>% 
  summarize(across(-region_4, median, na.rm = TRUE)) %>%
  mutate(region_4 = "INDO") %>% 
  mutate(Obs_solubility = Obs_sol_Fe/Obs_Fe, 
         v1_solubility = v1_sol_Fe / v1_Fe) %>%
  mutate(count = 99)

npac_row <- coal.ash.sims.1 %>%
  filter(region_4 %in% c("ENPAC", "WNPAC")) %>%
  dplyr::select(-matches(c("count"))) %>% 
  summarize(across(-region_4, median, na.rm = TRUE)) %>%
  mutate(region_4 = "NPAC") %>% 
  mutate(Obs_solubility = Obs_sol_Fe/Obs_Fe, 
         v1_solubility = v1_sol_Fe / v1_Fe) %>%
  mutate(count = 87)

# Add the new INDO row to the original dataset
coal.ash.sims.1 <- bind_rows(coal.ash.sims.1, indo_row, npac_row) %>% filter(region_4 != "ARCT", region_4 != "NA")

# need to add standard deviation for obs and model 
coal.ash.sims.2 <- coal.ash.sims.1 %>%
  mutate(xmin = (Obs_Fe-Obs_Fe_sd), xmax = (Obs_Fe+Obs_Fe_sd),
         ymin_v1 = (v1_Fe-v1_Fe_sd), ymax_v1 = (v1_Fe+v1_Fe_sd),
         
         xmin_sol = (Obs_sol_Fe-Obs_sol_Fe_sd), xmax_sol = (Obs_sol_Fe+Obs_sol_Fe_sd),
         ymin_sol_v1 = (v1_sol_Fe-v1_sol_Fe_sd), ymax_sol_v1 = (v1_sol_Fe+v1_sol_Fe_sd),
         
         xmin_solubility = (Obs_solubility-`Fe solubility_sd`), xmax_solubility = (Obs_solubility+`Fe solubility_sd`),
         ymin_solubility_v1 = (v1_solubility-v1_solubility_sd), ymax_solubility_v1 = (v1_solubility+v1_solubility_sd)) %>% 
  mutate(across(everything(), ~ ifelse(. <= 0, 0.000001, .)))

# PLOT FUNCTIONS -------------------

create_soluble_iron_plot_single_points_v2 <- function(data, x_var, y_var, x_min, x_max, y_min, y_max, region, color_palette, count_col, x_label = "Iron Observations (ng m-3)", y_label = "Modeled Iron (ng m-3)", plot_title = "[Soluble Iron]") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    
    geom_errorbar(aes(x = .data[[x_var]], ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.5, color = "gray") +
    geom_errorbar(aes(y = .data[[y_var]], xmin = .data[[x_min]], xmax = .data[[x_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.5, color = "gray") +
    
    # Scatter points with dynamic aes mapping using .data[[...]] for variables
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], 
                   color = .data[[region]], size = .data[[count_col]], 
                   shape = .data[["region_4"]])) + 
    
    scale_color_manual(values = color_palette) +
    scale_size(range = c(4, 20)) +
    scale_shape_manual(values = India.shapes) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.01)) + 
    scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    coord_cartesian(ylim = c(0.001, 100), xlim = c(0.001, 100)) +  
    
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 14, face = "bold"),
                       axis.title.x = element_text(size = 13, face = "bold"),
                       axis.title.y = element_text(size = 13, face = "bold"),
                       axis.text.x = element_text(size = 12, face = "bold"),
                       axis.text.y = element_text(size = 12, face = "bold")) +
    
    labs(x = x_label, y = y_label, title = plot_title) + 
    theme(
      
    )
}

create_total_iron_plot_single_points_v2 <- function(data, x_var, y_var, x_min, x_max, y_min, y_max, region, color_palette, count_col, x_label = "Iron Observations (ng m-3)", y_label = "Modeled Iron (ng m-3)", plot_title = "[Total Iron]") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    
    geom_errorbar(aes(x = .data[[x_var]], ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.5, color = "gray") +
    geom_errorbar(aes(y = .data[[y_var]], xmin = .data[[x_min]], xmax = .data[[x_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.5, color = "gray") +
    
    # Scatter points with dynamic aes mapping using .data[[...]] for variables
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], 
                   color = .data[[region]], size = .data[[count_col]], 
                   shape = .data[["region_4"]])) + 
    
    scale_color_manual(values = color_palette) +
    scale_size(range = c(4, 20)) +
    scale_shape_manual(values = India.shapes) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.01)) + 
    scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    coord_cartesian(ylim = c(.1, 10000), xlim = c(.1, 10000)) +  
    
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold")) +
    
    
    labs(x = x_label, y = y_label, title = plot_title)
}

create_solubility_plot_single_points_v2 <- function(data, x_var, y_var, x_min, x_max, y_min, y_max, region, color_palette, count_col,
                                                    x_label = "Iron Observations (%)", y_label = "Modeled Fe (%)", plot_title = "Iron Solubility") {
  # Ensure count is numeric
  data[[count_col]] <- as.numeric(data[[count_col]])
  
  ggplot(data) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1.5, show.legend = F) + 
    geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    geom_abline(slope = 1, intercept = -1, color = "black", linetype = "dashed", size = 1, show.legend = F) +
    
    geom_errorbar(aes(x = .data[[x_var]], ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.5, color = "gray") +
    geom_errorbar(aes(y = .data[[y_var]], xmin = .data[[x_min]], xmax = .data[[x_max]]), 
                  width = 0.1, size = 1.1, alpha = 0.5, color = "gray") +
    
    # Scatter points with dynamic aes mapping using .data[[...]] for variables
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], 
                   color = .data[[region]], size = .data[[count_col]], 
                   shape = .data[["region_4"]])) + 
    
    scale_color_manual(values = color_palette) +
    scale_size(range = c(4, 20)) +
    scale_shape_manual(values = India.shapes) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.01)) + 
    scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    coord_cartesian(ylim = c(0.001, .2), xlim = c(0.001, .2)) +  
    
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold")) +
    
    labs(x = x_label, y = y_label, title = plot_title)
}


# list out the following (df, "x_var", "y_var", region, color_palette, "count")
create_total_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_Fe", "v1_Fe", "xmin", "xmax", "ymin_v1", "ymax_v1", "region_4", India.palette,  "count")

create_soluble_iron_plot_single_points_v2(coal.ash.sims.2, "Obs_sol_Fe", "v1_sol_Fe", "xmin_sol", "xmax_sol", "ymin_sol_v1", "ymax_sol_v1", "region_4", India.palette, "count")

create_solubility_plot_single_points_v2(coal.ash.sims.2, "Obs_solubility", "v1_solubility", "xmin_solubility", "xmax_solubility", "ymin_solubility_v1", "ymax_solubility_v1", "region_4", India.palette, "count")



