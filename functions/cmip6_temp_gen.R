library(chillR)
library(dplyr)
library(tidyverse)

# CMIP6 ####

## Downloading climate data ####

location <- c(6.23,50.43) # Hürtgenwald

area_climate <- c(51,6,50,7)

download_cmip6_ecmwfr(scenarios = c("ssp245"),#c("ssp126", "ssp245", "ssp370", "ssp585"), 
                      area =  area_climate,
                      user = '294344',
                      key = '8ef99a9e-2ab8-424c-87a2-10ee5731d36f',
                      model = 'default',
                      frequency = 'monthly',
                      variable = c('Tmin', 'Tmax'),
                      year_start = 2015,
                      year_end = 2065
                      )


download_baseline_cmip6_ecmwfr(area = area_climate,
                               user = '294344',
                               key = '8ef99a9e-2ab8-424c-87a2-10ee5731d36f',
                               model = "match_downloaded",
                               frequency = "monthly",
                               variable = c("Tmin", "Tmax"),
                               year_start = 1986, 
                               year_end = 2014, # Does not work when > 2014
                               month = 1:12
                               )


## Creating change scenario ####

station_list <- handle_dwd(action = "list_stations",
                           location= c (6.23,50.43), # Hürtgenwald
                           time_interval = c(19900101,20231231)
                           )
write.csv(station_list, "data/climate_data/station_list.csv", row.names = FALSE)

### Schneifelforsthaus
station_02 <- data.frame(station_name = c(station_list$Station_name[2]),
                       longitude = c(station_list$Longitude[2]),
                       latitude = c(station_list$Latitude[2])
                       )

extracted_02 <- extract_cmip6_data(stations = station_02)

change_scenarios_02 <- gen_rel_change_scenario(extracted_02, 
                                               scenarios = seq(2025, 2050, 5)
                                               )

write.csv(change_scenarios_02, "data/all_change_scenarios_02.csv", row.names = FALSE)

scen_list_02 <- convert_scen_information(change_scenarios_02, give_structure = FALSE)

### Kall-Sisrig
station_03 <- data.frame(station_name = c(station_list$Station_name[3]),
                         longitude = c(station_list$Longitude[3]),
                         latitude = c(station_list$Latitude[3])
                         )

extracted_03 <- extract_cmip6_data(stations = station_03)

change_scenarios_03 <- gen_rel_change_scenario(extracted_03, 
                                               scenarios = seq(2025, 2050, 5)
                                               )

write.csv(change_scenarios_03, "data/all_change_scenarios_03.csv", row.names = FALSE)

scen_list_03 <- convert_scen_information(change_scenarios_03, give_structure = FALSE)


## Downloading weather ####

### Roth bei Prüm # Only wind data
#weather_raw_01_Roth <- handle_dwd(action = "download_weather", # Only wind data
#                                  location=  station_list$Station_ID[1],
#                                  time_interval = c(19900101,20221231)
#                                  )

### Schneifelforsthaus
weather_raw_02_Schneifel <- handle_dwd(action = "download_weather",
                                       location =  "04508", #station_list$Station_ID[2]  does not work when loaded from CSV the first Zero is lost 
                                       time_interval = c(19780101,20221231)
)

write.csv(weather_raw_02_Schneifel, "data/climate_data/weather_raw_02_Schneifel.csv", row.names = FALSE)

### Kall-Sisrig
weather_raw_03_Kall <- handle_dwd(action = "download_weather",
                                  location=  "02497", # station_list$Station_ID[3] does not work when loaded from CSV the first Zero is lost
                                  time_interval = c(19780101,20221231)
                                  )

write.csv(weather_raw_03_Kall, "data/climate_data/weather_raw_03_Kall.csv", row.names = FALSE)


## cleaning weather data ####

### Schneifelforsthaus
cleaned_weather_02_Schneifel_list <- handle_dwd(weather_raw_02_Schneifel)

cleaned_weather_02_Schneifel <- data.frame(cleaned_weather_02_Schneifel_list)

colnames(cleaned_weather_02_Schneifel) <- c("Station_ID", "YEARMODA", "Year", "Month", "Day", "Tmin", "Tmean", "Tmax", "Rainfall")

write.csv(cleaned_weather_02_Schneifel, "data/climate_data/cleaned_weather_02_Schneifel.csv", row.names = FALSE)

### Kall-Sisrig
cleaned_weather_03_Kall_list <- handle_dwd(weather_raw_03_Kall)

cleaned_weather_03_Kall <- data.frame(cleaned_weather_03_Kall_list)

colnames(cleaned_weather_03_Kall) <- c("Station_ID", "YEARMODA", "Year", "Month", "Day", "Tmin", "Tmean", "Tmax", "Rainfall")

write.csv(cleaned_weather_03_Kall, "data/climate_data/cleaned_weather_03_Kall.csv", row.names = FALSE)



## Fixing temperature data ####

### Schneifelforsthaus
fixed_weather_02_Schneifel <- fix_weather(cleaned_weather_02_Schneifel) # very small amount missing in one year 

write.csv(fixed_weather_02_Schneifel$weather, "data/climate_data/fixed_weather_02_Schneifel.csv", row.names = FALSE)

### Kall-Sisrig
fixed_weather_03_Kall <- fix_weather(cleaned_weather_03_Kall) # no data missing

write.csv(fixed_weather_03_Kall$weather, "data/climate_data/fixed_weather_03_Kall.csv", row.names = FALSE)



## Generating Temperature scenarios ####

### Schneifelforsthaus
weather_02_Schneifel <- read.csv("data/climate_data/fixed_weather_02_Schneifel.csv") 

temps_02 <- temperature_generation(weather_02_Schneifel, 
                                   years = c(1978, 2022), 
                                   sim_years = c(2001, 2100), 
                                   scen_list_02,
                                   seed = 52393)
save_temperature_scenarios(temps_02,
                           "data/future_climate_Schneifel",
                           "Schneifelhaus_futuretemps")

### Kall-Sisrig
weather_03_Kall <- read.csv("data/climate_data/fixed_weather_03_Kall.csv") 

temps_03 <- temperature_generation(weather_03_Kall, 
                                   years = c(1978, 2022), 
                                   sim_years = c(2001, 2100), 
                                   scen_list_03,
                                   seed = 52393)
save_temperature_scenarios(temps_03,
                           "data/future_climate_Kall",
                           "KallSisrig_futuretemps")




