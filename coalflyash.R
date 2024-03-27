setwd("Q:\\My Drive\\Code Repositories\\R\\coalflyash")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork);library(readxl)
#install.packages("")

# Reading in the data ----------------------------------------------------------------
data.file <- "Q:\\My Drive\\Collaborations\\Coal Fly Ash\\data\\FeObs_Hamilton2022.xlsx"

# Read in observational data (excel file) with specified parameters
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

