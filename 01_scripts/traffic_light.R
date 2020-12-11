# Read data for dynamic traffic light -----
# 

# source("01_scripts/read_data_worldometer.R")
#source("01_scripts/read_data_covid19datahub.R")

traffic_light <- function() {
  
  # data_confirmed <- fread("02_data/time_series_19-covid-Confirmed.csv")
  data_trafficlight <- fread("https://raw.githubusercontent.com/branmora/covid-19-cusco/master/data/traffic_light.csv")
}