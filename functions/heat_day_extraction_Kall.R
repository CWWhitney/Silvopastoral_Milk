library(chillR)
library(stringr)
library(dplyr)
# Load Weather ####
temps <- load_temperature_scenarios("data/future_climate_Kall", "Kall")

# Merge data in to one data frame ####
temps_merged <- data.frame(ssp = character(), 
                           model = character(), 
                           scenario = numeric(), 
                           DATE = character(), 
                           Year = integer(), 
                           Month = integer(), 
                           Day = integer(), 
                           nodata = integer(), 
                           Tmin = numeric(), 
                           Tmax = numeric())

for (i in 1:length(temps)){
  names <- unlist(str_split(names(temps[i]), "\\."))
  temps_with_category <- data.frame(ssp = names[2], model = names[3], scenario = as.integer(names[4]), temps[[i]])
  temps_merged <- rbind(temps_merged, temps_with_category)
}

# Create empty output ####
heat_above <- data.frame(ssp = character(), scenario = numeric(), 
                         dq5_25 = numeric(), dq5_26 = numeric(), dq5_27 = numeric(), dq5_28 = numeric(), dq5_29 = numeric(), dq5_30 = numeric(), dq5_31 = numeric(), dq5_32 = numeric(), dq5_33 = numeric(), dq5_34 = numeric(), dq5_35 = numeric(), dq5_36 = numeric(),
                         dmedian_25 = numeric(), dmedian_26 = numeric(), dmedian_27 = numeric(), dmedian_28 = numeric(), dmedian_29 = numeric(), dmedian_30 = numeric(), dmedian_31 = numeric(), dmedian_32 = numeric(), dmedian_33 = numeric(), dmedian_34 = numeric(), dmedian_35 = numeric(), dmedian_36 = numeric(),
                         dmean_25 = numeric(), dmean_26 = numeric(), dmean_27 = numeric(), dmean_28 = numeric(), dmean_29 = numeric(), dmean_30 = numeric(), dmean_31 = numeric(), dmean_32 = numeric(), dmean_33 = numeric(), dmean_34 = numeric(), dmean_35 = numeric(), dmean_36 = numeric(),
                         dq95_25 = numeric(), dq95_26 = numeric(), dq95_27 = numeric(), dq95_28 = numeric(), dq95_29 = numeric(), dq95_30 = numeric(), dq95_31 = numeric(), dq95_32 = numeric(), dq95_33 = numeric(), dq95_34 = numeric(), dq95_35 = numeric(), dq95_36 = numeric())

# Fill output ####
for (ssp in unique(temps_merged$ssp)){
  print(ssp)
  for (scenario in unique(temps_merged$scenario)){
    print(scenario)
    heat_above[nrow(heat_above)+1, "ssp"] <- ssp #add one row on first insert
    heat_above[nrow(heat_above), "scenario"] <- scenario
    for (i in 25:36){
      if (i < 36){
        heat_above[nrow(heat_above), paste0("dq5_", i)] <- quantile(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                              n = sum(Tmax >= i & Tmax < i+1), .by = c("Year", "model"))$n, 0.05, na.rm = TRUE)
      }else{
        heat_above[nrow(heat_above), paste0("dq5_", i)] <- quantile(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                              n = sum(Tmax >= i), .by = c("Year", "model"))$n, 0.05, na.rm = TRUE)
      }
    }
    for (i in 25:36){
      if (i < 36){
        heat_above[nrow(heat_above), paste0("dmedian_", i)] <- quantile(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                                  n = sum(Tmax >= i & Tmax < i+1), .by = c("Year", "model"))$n, 0.5, na.rm = TRUE)
      }else{
        heat_above[nrow(heat_above), paste0("dmedian_", i)] <- quantile(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                                  n = sum(Tmax >= i), .by = c("Year", "model"))$n, 0.5, na.rm = TRUE)
      }
    }
    for (i in 25:36){
      if (i < 36){
        heat_above[nrow(heat_above), paste0("dq95_", i)] <- quantile(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                               n = sum(Tmax >= i & Tmax < i+1), .by = c("Year", "model"))$n, 0.95, na.rm = TRUE)
      }else{
        heat_above[nrow(heat_above), paste0("dq95_", i)] <- quantile(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                               n = sum(Tmax >= i), .by = c("Year", "model"))$n, 0.95, na.rm = TRUE)
      }
    }
    for (i in 25:36){
      if (i < 36){  
        heat_above[nrow(heat_above), paste0("dmean_", i)] <- mean(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                            n = sum(Tmax >= i & Tmax < i+1), .by = c("Year", "model"))$n, na.rm = TRUE)
      }else{
        heat_above[nrow(heat_above), paste0("dmean_", i)] <- mean(summarise(temps_merged[temps_merged$scenario == scenario,],
                                                                            n = sum(Tmax >= i), .by = c("Year", "model"))$n, na.rm = TRUE)
      }
    }
  }
}

# Save output####
write.csv(heat_above, file = "data/heatdays_above_x_Kall.csv", row.names = FALSE)
