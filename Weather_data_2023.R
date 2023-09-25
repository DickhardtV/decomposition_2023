library(ggplot2)
library(data.table)

# Set working directory to the projekt
setwd("D:/Uni/0_Master_Pys_Geogr/3_Semester/Arctic Ecosystems/0_Projektwork")
getwd()

# Reading in the table
weather_files <- list.files("./data/weather", pattern = ".csv", full.names = TRUE)

tables <- list()

for (i in 1:length(weather_files)){
  name_suffix <- substr(basename(weather_files[i]), 17, 22)
  tables[[name_suffix]] <- read.table(file = weather_files[i], 
                    header = TRUE, dec=".", sep=";", skip = 10)
  
}

for (i in 1:length(weather_files)) {
  # Extract the 17th to 22nd letters of the file name
  name_suffix <- substr(basename(weather_files[i]), 17, 22)
  
  # Read the table
  table_data <- read.table(file = weather_files[i],
                           header = TRUE, dec=".", sep=";", skip = 10)  # Adjust for your file format
  
  # Check if the table with this name already exists in the list
  if (name_suffix %in% names(tables)) {
    # Combine the existing table with the new table
    tables[[name_suffix]] <- rbindlist(list(tables[[name_suffix]], table_data))
  } else {
    # If the name doesn't exist, create a new entry in the list
    tables[[name_suffix]] <- table_data
  }
}

tab1 <- tables[[1]]
