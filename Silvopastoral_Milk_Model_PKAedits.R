
# Packages ####

library(chillR)
library(decisionSupport)
#library(DiagrammeR)
#library(DiagrammeRsvg)
library(ggplot2)
#library(rsvg)
library(tidyverse)


# Input table ####
input_table <- read.csv("data/inputs.csv", stringsAsFactors = FALSE)

# Setting seed 

#set.seed(52393)

## Testing input table in function steps 
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n) # https://github.com/CWWhitney/Decision_Analysis_Course
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}

make_variables(as.estimate(input_table))


# Intervention Model ####

dairy_model <- function(x, varnames) {
  
  # Factors affecting milk production ####
  
  ## Calculate effect of Temperature Humidity index (THI) on milk production
  # Calculate THI based on maximum temperature and relative humidity
  Tmax <- c(25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36) # in °C
  RH <- c(25, 30, 35, 40, 45, 50, 55, 60) # in %
  RH <- 75
  
  # Create a dataframe of all combinations
  combinations <- expand.grid(Tmax = Tmax, RH = RH)
  # Calculate THI using the formula: (1.8 * Tmaxi + 32) - ((0.55 - 0.0055 * HR) * (1.8 * Tmaxi - 26.8)) #National Research Council (1971)
  combinations$THI <- with(combinations, ((1.8 * Tmax + 32) - ((0.55 - 0.0055*RH) * (1.8*Tmax - 26))))
  
  # Filter out rows where THI is less than 68 - Though a lot of studies use THI >72, 
  # there are some studies that have shown that milk production is affected after THI >68 
  # in some breeds like Italien Holstein. Since we want the code to be as general and 
  # encompassing uncertainty as possible 68 is used as the low threshold in this study
  filtered_combinations <- subset(combinations, THI >= 67)
  filtered_combinations$per_reduction <- ((filtered_combinations$THI - 67) * perc_milk_reduction_heat_THI_kg_cow_day)/100
  
  # milk production affected by heat stress - Reduction in milk yield per THI point >68
  #THI_values <- seq(68, max(filtered_combinations$THI), by = 1)
  
  # Baseline####
  # Existing grassland with dairy production
  # caluclate number cows that can be grazed on the field
  grass_area <- production_area
  cows_ha_pot <- ceiling(cows_ha * grass_area * cows_expected_tobemilked) # In well-managed herds, 85% of the cows are expected to be milked; 
  #any decrease in the proportion of milking cows denotes a lack of calving homogeneity across the year.
  
  # milk production
  ## Potential Milk produced  per hectare per day
  milk_per_cow <-vv((milk_production_kg_cow_day), var_CV, n_years) 
  milk_kg_ha_pot <- milk_per_cow * cows_ha_pot
  
  ## Milk production from grazing
  ##! need not be the same every year -  better way to account for uncertainity
  milk_grazig_perc <- vv(milk_production_grazing_perc, var_CV, n_years)
  milk_kg_ha_grazing_pot <-  milk_kg_ha_pot * milk_grazig_perc
  
  ## Milk production affected by drought
  milk_kg_ha_rest_pot <- milk_kg_ha_pot - milk_kg_ha_grazing_pot
  
  ## Milk production affected by THI
  milk_kg_ha_THI <- milk_kg_ha_rest_pot - filtered_combinations$per_reduction

  # calculate the number of days cows will experience heat stress based on CMIP6 data
  ## Heat stress #
  ## Calculate the number of days heat stress is present in the future 
  heat_table_no_AF <- data_frame(heat_stress_days_25GC = c(heat_table_1$stress_days_25GC_2025[c(1:5)],
                                                             heat_table_1$stress_days_25GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_25GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_25GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_25GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_25GC_2050[c(26:30)]),
                                   heat_stress_days_26GC = c(heat_table_1$stress_days_26GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_26GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_26GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_26GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_26GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_26GC_2050[c(26:30)]),
                                   heat_stress_days_27GC = c(heat_table_1$stress_days_27GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_27GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_27GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_27GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_27GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_27GC_2050[c(26:30)]),
                                   heat_stress_days_28GC = c(heat_table_1$stress_days_28GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_28GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_28GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_28GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_28GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_28GC_2050[c(26:30)]),
                                   heat_stress_days_29GC = c(heat_table_1$stress_days_29GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_29GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_29GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_29GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_29GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_29GC_2050[c(26:30)]),
                                   heat_stress_days_30GC = c(heat_table_1$stress_days_30GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_30GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_30GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_30GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_30GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_30GC_2050[c(26:30)]),
                                   heat_stress_days_31GC = c(heat_table_1$stress_days_31GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_31GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_31GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_31GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_31GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_31GC_2050[c(26:30)]),
                                   heat_stress_days_32GC = c(heat_table_1$stress_days_32GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_32GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_32GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_32GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_32GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_32GC_2050[c(26:30)]),
                                   heat_stress_days_33GC = c(heat_table_1$stress_days_33GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_33GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_33GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_33GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_33GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_33GC_2050[c(26:30)]),
                                   heat_stress_days_34GC = c(heat_table_1$stress_days_34GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_34GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_34GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_34GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_34GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_34GC_2050[c(26:30)]),
                                   heat_stress_days_35GC = c(heat_table_1$stress_days_35GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_35GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_35GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_35GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_35GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_35GC_2050[c(26:30)]),
                                   heat_stress_days_36GC = c(heat_table_1$stress_days_36GC_2025[c(1:5)], 
                                                             heat_table_1$stress_days_36GC_2030[c(6:10)], 
                                                             heat_table_1$stress_days_36GC_2035[c(11:15)], 
                                                             heat_table_1$stress_days_36GC_2040[c(16:20)],
                                                             heat_table_1$stress_days_36GC_2045[c(21:25)], 
                                                             heat_table_1$stress_days_36GC_2050[c(26:30)])
  )
  # calculate days without heat stress
  heat_table_no_AF <- heat_table_no_AF %>% mutate(normal_days = pmax(365 - rowSums(select(., starts_with("heat_stress_days"))), 0))
  
  # Ensure the length of milk_kg_ha_rest_pot matches the number of rows in the data frame
  if (length(milk_kg_ha_rest_pot) != nrow(heat_table_no_AF)) {
    stop("The length of milk_production must match the number of rows in heat_table_no_AF.")
  }
  # calculate milk production on days without heat stress in a year
  heat_table_no_AF <- heat_table_no_AF %>% mutate(normal_production_kg = normal_days * milk_kg_ha_rest_pot)
  
  #heat_table_no_AF <- unname(as.matrix(heat_table_no_AF_1))


  THI_table_stress_sum_no_AF <- heat_table_no_AF * THI_table
  
  ## Costs####
  
  
  ## Benefits####
  
  ## Bottomline Profit ####
  
  
  
  # Agroforestry System ####
  
  # Considering Ecological aspect for the calculation "TRUE" or not "FALSE
  #ecological_aspects_used <-  c(rep("NO",n_years)) #c(rep("YES", n_years))
    
  agroforestry_area <-  agroforestry_area_percentage *  production_area
  
  # Grassland yield factor with AF 
  ##! I don't understand this logic
  # reduction in milk production based grass yield due to implementation of AF - but why??
  # poplar leaves or trimmings can be given - this would be important only if farmer is selling the grass
  # https://www.innovativefarmers.org/knowledge-hub/which-tree-species-can-provide-nutritional-value-to-my-livestock/
  # https://www.poplarandwillow.org.nz/news/2020/poplars-and-willows-offer-a-valuable-feed-source-during-dry-conditions
  # I would rather consider the cost of extra feed than reduce milk production.
  # grass_yield_fac_AF <- vv(ifelse(AF_area_perc < 0.1, AF_on_milk_production_AF_5_10_fac, 
  #                                 ifelse(AF_area_perc < 0.15, AF_on_milk_production_AF_10_15_fac, 
  #                                        ifelse(AF_area_perc < 0.20, AF_on_milk_production_AF_15_20_fac,
  #                                               AF_on_milk_production_AF_20_25_fac
  #                                        ))), var_CV, n_years)
  
  ## Grass area                    
  grass_area_AF <- production_area - agroforestry_area
  cows_ha_pot_AF <- cows_ha * grass_area_AF
  ## Cows per hectare potential 
  # cows_ha_pot <-  vv(cows_ha, var_CV, n_years) 
  # cows_ha_no_AF <- cows_ha_pot
  # cows_ha_AF <- cows_ha_pot * grass_yield_fac_AF
  
  ## Potential Milk produced  per hectare in an AF system
  milk_t_ha_pot_AF <- milk_per_cow * cows_ha_pot_AF
  # The data of milk yield, is current production data, losses modeled included 
  ##! What losses and risks? the variation is kind of accounted for by using VV
  #milk_t_ha_pot_loss_comp <- milk_t_ha_pot * (1 + vv(milk_loss_incl_production_perc, var_CV, n_years)) 
  
  ## Milk production from grazing
  milk_t_ha_grazing_pot_AF <-  milk_t_ha_pot_AF * milk_grazig_perc
  ## Milk production affected by drought
  milk_t_ha_rest_pot_AF <- milk_t_ha_pot_AF - milk_t_ha_grazing_pot_AF
  
  # Calculate effect of Temperature Humidity index (THI) on milk production
  milk_t_ha_rest_pot_AF <- milk_t_ha_rest_pot_AF - ((THI_values$THI_values - 67)*perc_milk_reduction_heat_THI_kg_cow_day)
  
  # calculate the number of days cows will experience heat stress based on CMIP6 data and presence of AF system
  
  ## Potential yield of Poplar per hectare
  yield_poplar_MS_taDM_ha_pot <-  vv(yield_poplar_MS_taDM_ha_a, var_CV, n_years) # Using medium sight conditions, moderate or bad soil condition but moderate or much rain, long periods with no rain have a great effect
  #### SRC harvest frequency to be taken inot ocnsidereation and accordingly replanting 
  ##### and the effect on AF on number of heast stress days and therefore THI and milk producion
  
  ## Heat stress
  heat_table_1 <- data.frame(stress_days_25GC_2025 = vv(heat_stress_days_25GC_2025, var_CV, n_years),
                             stress_days_26GC_2025 = vv(heat_stress_days_26GC_2025, var_CV, n_years),
                             stress_days_27GC_2025 = vv(heat_stress_days_27GC_2025, var_CV, n_years),
                             stress_days_28GC_2025 = vv(heat_stress_days_28GC_2025, var_CV, n_years),
                             stress_days_29GC_2025 = vv(heat_stress_days_29GC_2025, var_CV, n_years),
                             stress_days_30GC_2025 = vv(heat_stress_days_30GC_2025, var_CV, n_years),
                             stress_days_31GC_2025 = vv(heat_stress_days_31GC_2025, var_CV, n_years),
                             stress_days_32GC_2025 = vv(heat_stress_days_32GC_2025, var_CV, n_years),
                             stress_days_33GC_2025 = vv(heat_stress_days_33GC_2025, var_CV, n_years),
                             stress_days_34GC_2025 = vv(heat_stress_days_34GC_2025, var_CV, n_years),
                             stress_days_35GC_2025 = vv(heat_stress_days_35GC_2025, var_CV, n_years),
                             stress_days_36GC_2025 = vv(heat_stress_days_36GC_2025, var_CV, n_years),
                             stress_days_25GC_2030 = vv(heat_stress_days_25GC_2030, var_CV, n_years),
                             stress_days_26GC_2030 = vv(heat_stress_days_26GC_2030, var_CV, n_years),
                             stress_days_27GC_2030 = vv(heat_stress_days_27GC_2030, var_CV, n_years),
                             stress_days_28GC_2030 = vv(heat_stress_days_28GC_2030, var_CV, n_years),
                             stress_days_29GC_2030 = vv(heat_stress_days_29GC_2030, var_CV, n_years),
                             stress_days_30GC_2030 = vv(heat_stress_days_30GC_2030, var_CV, n_years),
                             stress_days_31GC_2030 = vv(heat_stress_days_31GC_2030, var_CV, n_years),
                             stress_days_32GC_2030 = vv(heat_stress_days_32GC_2030, var_CV, n_years),
                             stress_days_33GC_2030 = vv(heat_stress_days_33GC_2030, var_CV, n_years),
                             stress_days_34GC_2030 = vv(heat_stress_days_34GC_2030, var_CV, n_years),
                             stress_days_35GC_2030 = vv(heat_stress_days_35GC_2030, var_CV, n_years),
                             stress_days_36GC_2030 = vv(heat_stress_days_36GC_2030, var_CV, n_years),
                             stress_days_25GC_2035 = vv(heat_stress_days_25GC_2035, var_CV, n_years),
                             stress_days_26GC_2035 = vv(heat_stress_days_26GC_2035, var_CV, n_years),
                             stress_days_27GC_2035 = vv(heat_stress_days_27GC_2035, var_CV, n_years),
                             stress_days_28GC_2035 = vv(heat_stress_days_28GC_2035, var_CV, n_years),
                             stress_days_29GC_2035 = vv(heat_stress_days_29GC_2035, var_CV, n_years),
                             stress_days_30GC_2035 = vv(heat_stress_days_30GC_2035, var_CV, n_years),
                             stress_days_31GC_2035 = vv(heat_stress_days_31GC_2035, var_CV, n_years),
                             stress_days_32GC_2035 = vv(heat_stress_days_32GC_2035, var_CV, n_years),
                             stress_days_33GC_2035 = vv(heat_stress_days_33GC_2035, var_CV, n_years),
                             stress_days_34GC_2035 = vv(heat_stress_days_34GC_2035, var_CV, n_years),
                             stress_days_35GC_2035 = vv(heat_stress_days_35GC_2035, var_CV, n_years),
                             stress_days_36GC_2035 = vv(heat_stress_days_36GC_2035, var_CV, n_years),
                             stress_days_25GC_2040 = vv(heat_stress_days_25GC_2040, var_CV, n_years),
                             stress_days_26GC_2040 = vv(heat_stress_days_26GC_2040, var_CV, n_years),
                             stress_days_27GC_2040 = vv(heat_stress_days_27GC_2040, var_CV, n_years),
                             stress_days_28GC_2040 = vv(heat_stress_days_28GC_2040, var_CV, n_years),
                             stress_days_29GC_2040 = vv(heat_stress_days_29GC_2040, var_CV, n_years),
                             stress_days_30GC_2040 = vv(heat_stress_days_30GC_2040, var_CV, n_years),
                             stress_days_31GC_2040 = vv(heat_stress_days_31GC_2040, var_CV, n_years),
                             stress_days_32GC_2040 = vv(heat_stress_days_32GC_2040, var_CV, n_years),
                             stress_days_33GC_2040 = vv(heat_stress_days_33GC_2040, var_CV, n_years),
                             stress_days_34GC_2040 = vv(heat_stress_days_34GC_2040, var_CV, n_years),
                             stress_days_35GC_2040 = vv(heat_stress_days_35GC_2040, var_CV, n_years),
                             stress_days_36GC_2040 = vv(heat_stress_days_36GC_2040, var_CV, n_years),
                             stress_days_25GC_2045 = vv(heat_stress_days_25GC_2045, var_CV, n_years),
                             stress_days_26GC_2045 = vv(heat_stress_days_26GC_2045, var_CV, n_years),
                             stress_days_27GC_2045 = vv(heat_stress_days_27GC_2045, var_CV, n_years),
                             stress_days_28GC_2045 = vv(heat_stress_days_28GC_2045, var_CV, n_years),
                             stress_days_29GC_2045 = vv(heat_stress_days_29GC_2045, var_CV, n_years),
                             stress_days_30GC_2045 = vv(heat_stress_days_30GC_2045, var_CV, n_years),
                             stress_days_31GC_2045 = vv(heat_stress_days_31GC_2045, var_CV, n_years),
                             stress_days_32GC_2045 = vv(heat_stress_days_32GC_2045, var_CV, n_years),
                             stress_days_33GC_2045 = vv(heat_stress_days_33GC_2045, var_CV, n_years),
                             stress_days_34GC_2045 = vv(heat_stress_days_34GC_2045, var_CV, n_years),
                             stress_days_35GC_2045 = vv(heat_stress_days_35GC_2045, var_CV, n_years),
                             stress_days_36GC_2045 = vv(heat_stress_days_36GC_2045, var_CV, n_years),
                             stress_days_25GC_2050 = vv(heat_stress_days_25GC_2050, var_CV, n_years),
                             stress_days_26GC_2050 = vv(heat_stress_days_26GC_2050, var_CV, n_years),
                             stress_days_27GC_2050 = vv(heat_stress_days_27GC_2050, var_CV, n_years),
                             stress_days_28GC_2050 = vv(heat_stress_days_28GC_2050, var_CV, n_years),
                             stress_days_29GC_2050 = vv(heat_stress_days_29GC_2050, var_CV, n_years),
                             stress_days_30GC_2050 = vv(heat_stress_days_30GC_2050, var_CV, n_years),
                             stress_days_31GC_2050 = vv(heat_stress_days_31GC_2050, var_CV, n_years),
                             stress_days_32GC_2050 = vv(heat_stress_days_32GC_2050, var_CV, n_years),
                             stress_days_33GC_2050 = vv(heat_stress_days_33GC_2050, var_CV, n_years),
                             stress_days_34GC_2050 = vv(heat_stress_days_34GC_2050, var_CV, n_years),
                             stress_days_35GC_2050 = vv(heat_stress_days_35GC_2050, var_CV, n_years),
                             stress_days_36GC_2050 = vv(heat_stress_days_36GC_2050, var_CV, n_years)
  )
  # Assign 0 if the values are negative
  heat_table_1[heat_table_1 <= 0] <- 0
  
  
  #Yet to be checked ####
  
  
  ## Heat stress days AF ####
  
  AF_on_stress_days_raw_1 <- data_frame(AF_on_stress_days_25GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_25GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_25GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_25GC_15_20,
                                                                                         AF_on_days_25GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_26GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_26GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_26GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_26GC_15_20,
                                                                                         AF_on_days_26GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_27GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_27GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_27GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_27GC_15_20,
                                                                                         AF_on_days_27GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_28GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_28GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_28GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_28GC_15_20,
                                                                                         AF_on_days_28GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_29GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_29GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_29GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_29GC_15_20,
                                                                                         AF_on_days_26GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_30GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_30GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_30GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_30GC_15_20,
                                                                                         AF_on_days_30GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_31GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_31GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_31GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_31GC_15_20,
                                                                                         AF_on_days_31GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_32GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_32GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_32GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_32GC_15_20,
                                                                                         AF_on_days_32GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_33GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_33GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_33GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_33GC_15_20,
                                                                                         AF_on_days_33GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_34GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_34GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_34GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_34GC_15_20,
                                                                                         AF_on_days_30GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_35GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_35GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_35GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_35GC_15_20,
                                                                                         AF_on_days_35GC_20_25
                                                                                  ))), var_CV, n_years),
                                        AF_on_stress_days_36GC = vv(ifelse(AF_area_perc < 0.1, AF_on_days_36GC_5_10, 
                                                                           ifelse(AF_area_perc < 0.15, AF_on_days_36GC_10_15, 
                                                                                  ifelse(AF_area_perc < 0.20, AF_on_days_36GC_15_20,
                                                                                         AF_on_days_36GC_20_25
                                                                                  ))), var_CV, n_years),
  )
  
  
  
  AF_on_stress_days_raw_2 <- AF_on_stress_days <- unname(as.matrix(AF_on_stress_days_raw_1))
  
  AF_on_stress_days_raw <- cbind(c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years)),
                                 c(rep(0, n_years))
  )
  # reducing AF effect on heat stress based on poplar growth stage
  for(y in 1:n_years) {
    
    for(x in 1:12) {
      if (y == 1) {
        AF_on_stress_days_raw[y,x] <- 0
      }
      if (y ==2) {
        AF_on_stress_days_raw[y,x] <- AF_on_stress_days_raw_2[y,x] * (1/3)
      }
      if (y ==3) {
        AF_on_stress_days_raw[y,x] <- AF_on_stress_days_raw_2[y,x] * (2/3)
      }
      if (y > 3) {
        AF_on_stress_days_raw[y,x] <- AF_on_stress_days_raw_2[y,x]
      }
    }
  }
  
# Function to round only to numeric columns to a given 'digits' decimal place, leaving other data types (like character or factor columns) unchanged.  
  round_stress_effect_AF <- function(x, digits) {
    numeric_columns <- sapply(x, mode) == "numeric"
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  AF_on_stress_days <- round_stress_effect_AF(AF_on_stress_days_raw, 0)
  
  

  
  
  heat_table_AF <- cbind(c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years)),
                         c(rep(0, n_years))
  )
  
  for(y in 1:n_years){
    
    for(x in 1:12){
      if (AF_on_stress_days[y,x] < 0){
        AF_on_stress_days[y,x] <- 0
      }
      if( x + AF_on_stress_days[y,x] <= 12){
        heat_table_AF[y,x] <- heat_table_no_AF[y,x+AF_on_stress_days[y,x]]
      }
    }
  }

  
  
  ## THI ####
  
  THI_table_1 <- data_frame(THI_stress_25GC = vv(THI_heat_stress_25GC, var_CV, n_years),
                            THI_stress_26GC = vv(THI_heat_stress_26GC, var_CV, n_years),
                            THI_stress_27GC = vv(THI_heat_stress_27GC, var_CV, n_years),
                            THI_stress_28GC = vv(THI_heat_stress_28GC, var_CV, n_years),
                            THI_stress_29GC = vv(THI_heat_stress_29GC, var_CV, n_years),
                            THI_stress_30GC = vv(THI_heat_stress_30GC, var_CV, n_years),
                            THI_stress_31GC = vv(THI_heat_stress_31GC, var_CV, n_years),
                            THI_stress_32GC = vv(THI_heat_stress_32GC, var_CV, n_years),
                            THI_stress_33GC = vv(THI_heat_stress_33GC, var_CV, n_years),
                            THI_stress_34GC = vv(THI_heat_stress_34GC, var_CV, n_years),
                            THI_stress_35GC = vv(THI_heat_stress_35GC, var_CV, n_years),
                            THI_stress_36GC = vv(THI_heat_stress_36GC, var_CV, n_years)
  )
  
  THI_table_1[THI_table_1 <= 0] <- 0
  
  THI_table <- unname(as.matrix(THI_table_1))
  
  THI_table_stress_sum_no_AF <- heat_table_no_AF * THI_table
  
  
  THI_table_stress_sum_AF <- heat_table_AF * THI_table
  
  ### Milk reduction THI 
  
  milk_reduction_THI_kg_cow_day <- vv(milk_reduction_heat_THI_kg_cow_day, var_CV, n_years)
  
  
  milk_reduction_stress_kg_cow_no_AF <- THI_table_stress_sum_no_AF * milk_reduction_THI_kg_cow_day
  
  
  milk_reduction_stress_kg_cow_AF <- THI_table_stress_sum_AF * milk_reduction_THI_kg_cow_day
  
  
  milk_reduction_kg_cow_sum_no_AF <- rowSums(milk_reduction_stress_kg_cow_no_AF)
  
  milk_reduction_kg_cow_sum_AF <- rowSums(milk_reduction_stress_kg_cow_AF)
  
  
  milk_reduction_t_ha_no_AF <- (milk_reduction_kg_cow_sum_no_AF * cows_ha_no_AF) / 1000
  
  milk_reduction_t_ha_AF <- (milk_reduction_kg_cow_sum_AF * cows_ha_no_AF) / 1000
  
  
  # Heat stress days on veterinary costs ####
  increased_veterinary_costs_heat_day_fac <-  cbind(vv(increased_veterinary_costs_heat_25GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_26GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_27GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_28GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_29GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_30GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_31GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_32GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_33GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_34GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_35GC_perc, var_CV, n_years),
                                                    vv(increased_veterinary_costs_heat_36GC_perc, var_CV, n_years)
  )
  
  
  veterinary_costs_t_milk_pot <-  vv(veterinary_costs_t_milk, var_CV, n_years)
  
  veterinary_costs_t_milk_pot_day <-  veterinary_costs_t_milk_pot / 365
  
  veterinary_costs_t_milk_heat_no_AF <- rowSums(heat_table_no_AF *
                                                  veterinary_costs_t_milk_pot_day *
                                                  increased_veterinary_costs_heat_day_fac
  )
  
  veterinary_costs_t_milk_heat_AF <-  rowSums(heat_table_AF *
                                                veterinary_costs_t_milk_pot_day *
                                                increased_veterinary_costs_heat_day_fac
  ) 
  
  
  # Auxiliary table ####
  auxiliary_table <- data.frame(year = c(1:n_years),
                                drought_happened = rep(0, n_years),
                                milk_t_ha_year_no_AF = rep(0, n_years),
                                milk_t_ha_year_AF = rep(0, n_years),
                                yield_poplar_MS_taDM_ha_year = rep(0, n_years),
                                yield_poplar_MS_taDM_ha_harvest_year = rep(0, n_years),
                                drought_effect_milk_no_AF = rep(0, n_years),
                                drought_effect_milk_AF = rep(0, n_years),
                                drought_effect_poplar_AF = rep(0, n_years),
                                drought_effect_poplar_died_happende = rep(0, n_years),
                                drought_effect_poplar_died_perc = rep(0, n_years),
                                drought_effect_poplar_died_yield_effect_perc  = rep(0 , n_years),
                                drought_effect_poplar_died_permanent_yield_multiplier = rep(0, n_years),
                                drought_effect_poplar_died_perc_1stY = rep(0, n_years),
                                drought_effect_poplar_died_perc_after_1stY = rep(0, n_years),
                                seed_costs_ha_pot = rep(0, n_years),
                                seed_costs_year_no_AF = rep(0, n_years),
                                seed_costs_year_AF = rep(0, n_years),
                                drought_effect_seed_costs_nY = rep(0, n_years),
                                AF_on_seed_costs_after_drought_nY = rep(0, n_years)
  )
  
  
  ## Drought event
  ## Drought happened
  auxiliary_table$drought_happened <- chance_event(drought_chance, 1, 0, n = n_years)
  
  ## Drought effects 
  ### Drought effects on milk production no AF
  auxiliary_table$drought_effect_milk_no_AF <-  vv(drought_effect_milk_production, var_CV, n_years)
  
  ### Drought effects on milk production AF 
  auxiliary_table$drought_effect_milk_AF <- auxiliary_table$drought_effect_milk_no_AF *
    (1 + 
       vv(AF_on_drought_effect_milk_production, var_CV, n_years)
    )
  ### Drought effects on  production AF
  auxiliary_table$drought_effect_poplar_AF <- vv(drought_effect_poplar_production, var_CV, n_years)
  
  auxiliary_table$drought_effect_poplar_died_perc_1stY <- c(drought_effect_poplar_died_perc_1stY, rep(0, (n_years - 1))) # Y1
  
  auxiliary_table$drought_effect_poplar_died_perc_after_1stY <-  c(0, vv(drought_effect_poplar_died_perc_after_1stY, var_CV, n_years - 1)) # Ya1
  
  auxiliary_table$drought_effect_poplar_died_yield_effect_perc <- vv(drought_effect_poplar_died_yield_effect_perc, var_CV, n_years)
  
  
  ## Seed coats
  auxiliary_table$seed_costs_ha_pot <- vv(seed_costs_ha, var_CV, n_years)
  
  auxiliary_table$drought_effect_seed_costs_nY <-  vv(drought_effect_seed_costs_next_year, var_CV, n_years)
  
  auxiliary_table$AF_on_seed_costs_after_drought_nY <-  1 - 
    vv(AF_on_seed_costs_after_drought, var_CV, n_years)
  
  
  
  
  # For years; for loop for drought ####
  for (y in 1:n_years) {
    
    ## Drought not depending in different Years ####
    if(auxiliary_table$drought_happened[y] == 1) {
      ### Drought effects no AF
      #### Reduced yield
      auxiliary_table$milk_t_ha_year_no_AF[y] <-  milk_t_ha_grazing_pot[y] * 
        auxiliary_table$drought_effect_milk_no_AF[y]  # No different effects on milk in first Y or later Y 
    }
    
    ## No drought happened 
    else{
      auxiliary_table$milk_t_ha_year_no_AF[y] <-  milk_t_ha_grazing_pot[y]
    } 
    
    
    ## For first Year ####
    if (y == 1){
      
      auxiliary_table$seed_costs_year_no_AF[y] <- auxiliary_table$seed_costs_ha_pot[y] 
      
      auxiliary_table$seed_costs_year_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
      
      
      ### Drought happened 1st Y
      if(auxiliary_table$drought_happened[y] == 1) {
        
        #### Drought effects no AF # done in Drought not depending in different Years
        
        #### Drought effect AF
        ##### Reduced yield
        auxiliary_table$milk_t_ha_year_AF[y] <- milk_t_ha_grazing_pot[y] *
          auxiliary_table$drought_effect_milk_no_AF[y] # No AF because trees are to slam in Y1
        
        #### Poplars dying
        auxiliary_table$drought_effect_poplar_died_happende[y] <- chance_event(drought_effect_poplar_died_chance_1stY, 1, 0)
        
        if(auxiliary_table$drought_effect_poplar_died_happende[y] == 1){
          
          auxiliary_table$drought_effect_poplar_died_perc[y] <-  auxiliary_table$drought_effect_poplar_died_perc_1stY[y]
        }
        else {
          auxiliary_table$drought_effect_poplar_died_perc[y] <- 0
        }
        
        #### Poplars dying effect on future yield
        if(auxiliary_table$drought_effect_poplar_died_happende[y] == 0) {     
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = 1
        }
        else {
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] =  1 - 
            (auxiliary_table$drought_effect_poplar_died_perc[y] * 
               auxiliary_table$drought_effect_poplar_died_yield_effect_perc [y])
        }
        
        #### Actual yield / biomass growth 1stY with drought 
        auxiliary_table$yield_poplar_MS_taDM_ha_year[y] <-  yield_poplar_MS_taDM_ha_pot[y] * 
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] *
          auxiliary_table$drought_effect_poplar_AF[y]
      }
      ### No drought happened 1Y
      else {
        # Potential milk production AF Y1
        auxiliary_table$milk_t_ha_year_AF[y] <- milk_t_ha_grazing_pot[y]
        
        #### Poplars not dying effect on future yield
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = 1
        
        #### Actual yield / biomass growth 1stY without drought 
        auxiliary_table$yield_poplar_MS_taDM_ha_year[y] <-  yield_poplar_MS_taDM_ha_pot[y] *
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y]
      }
      
    }
    
    
    # For years after the first ####
    else{
      
      ## Seed costs ####
      if(auxiliary_table$drought_happened[y-1] == 1) {
        ### Seed costs for the year after the drought 
        auxiliary_table$seed_costs_year_no_AF[y] <- auxiliary_table$seed_costs_ha_pot[y] + 
          auxiliary_table$seed_costs_ha_pot[y] *
          auxiliary_table$drought_effect_seed_costs_nY[y]
        
        auxiliary_table$seed_costs_year_AF[y] <-  auxiliary_table$seed_costs_ha_pot[y] + 
          auxiliary_table$seed_costs_ha_pot[y] * 
          (auxiliary_table$drought_effect_seed_costs_nY[y] *
             auxiliary_table$AF_on_seed_costs_after_drought_nY[y])
      }
      
      else {
        ## Seed costs without drought after Y1
        auxiliary_table$seed_costs_year_no_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
        
        auxiliary_table$seed_costs_year_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
      } 
      
      
      
      
      # Drought happened in Y after 1st
      if(auxiliary_table$drought_happened[y] == 1) {
        
        # Drought effects no AF
        
        #### Drought effect AF
        ##### Reduced yield
        auxiliary_table$milk_t_ha_year_AF[y] <- milk_t_ha_grazing_pot[y] *
          auxiliary_table$drought_effect_milk_AF[y]
        
        
        ## Poplars dying
        auxiliary_table$drought_effect_poplar_died_happende[y] <- chance_event(drought_effect_poplar_died_chance_after_1stY, 1, 0)
        
        if(auxiliary_table$drought_effect_poplar_died_happende[y] == 1){
          
          auxiliary_table$drought_effect_poplar_died_perc[y] <-  auxiliary_table$drought_effect_poplar_died_perc_after_1stY[y]
        }
        else {
          auxiliary_table$drought_effect_poplar_died_perc[y] <- 0
        }
        
        ## Poplars dying effect on future yield
        if(auxiliary_table$drought_effect_poplar_died_happende[y] == 0) {     
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y-1]
        }
        else {
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] =  auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y-1] - 
            (auxiliary_table$drought_effect_poplar_died_perc[y] * 
               auxiliary_table$drought_effect_poplar_died_yield_effect_perc [y])
        }
        ## Actual yield / biomass growth after 1stY with drought
        auxiliary_table$yield_poplar_MS_taDM_ha_year[y] <-  yield_poplar_MS_taDM_ha_pot[y] * 
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] *
          auxiliary_table$drought_effect_poplar_AF[y]
      }
      
      # No drought happened in Y after 1st
      else {
        if(auxiliary_table$drought_effect_poplar_died_happende[y] == 0) {    
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y-1]
        }
        ## No drought effect on milk production after 1stY
        auxiliary_table$milk_t_ha_year_AF[y] <- milk_t_ha_grazing_pot[y]
        
        ## Actual yield / biomass growth after 1stY without drought
        auxiliary_table$yield_poplar_MS_taDM_ha_year[y] <-  yield_poplar_MS_taDM_ha_pot[y] *
          auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y]
      }
    }
    
    # Harvest interval ####
    # Harvest intervals 10 years, year 5 every 2nd row, year 10 other rows, ... to keep the micro climate effects going 
    if (y == 5){
      auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (auxiliary_table$yield_poplar_MS_taDM_ha_year[1] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[2] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[3] +
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[4] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[5]) /
        2
    }
    if (y == 10){
      auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (auxiliary_table$yield_poplar_MS_taDM_ha_year[1] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[2] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[3] +
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[4] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[5]) /
        2 +
        (auxiliary_table$yield_poplar_MS_taDM_ha_year[6] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[7] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[8] +
           auxiliary_table$yield_poplar_MS_taDM_ha_year[9] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[10]) /
        2
    }
    if (y == 15){
      auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (auxiliary_table$yield_poplar_MS_taDM_ha_year[6] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[7] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[8] +
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[9] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[10]) /
        2 + 
        (auxiliary_table$yield_poplar_MS_taDM_ha_year[11] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[12] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[13] +
           auxiliary_table$yield_poplar_MS_taDM_ha_year[14] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[15]) /
        2
    }
    if (y == 20){
      auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (auxiliary_table$yield_poplar_MS_taDM_ha_year[11] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[12] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[13] +
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[14] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[15]) /
        2 + 
        (auxiliary_table$yield_poplar_MS_taDM_ha_year[16] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[17] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[18] +
           auxiliary_table$yield_poplar_MS_taDM_ha_year[19] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[20]) / 
        2
    }
    if (y == 25){
      auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (auxiliary_table$yield_poplar_MS_taDM_ha_year[16] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[17] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[18] +
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[19] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[20]) / 
        2 + 
        (auxiliary_table$yield_poplar_MS_taDM_ha_year[21] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[22] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[23] +
           auxiliary_table$yield_poplar_MS_taDM_ha_year[24] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[25]) / 
        2
    }
    if (y == 30){
      auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (auxiliary_table$yield_poplar_MS_taDM_ha_year[21] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[22] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[23] +
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[24] + 
                                                                    auxiliary_table$yield_poplar_MS_taDM_ha_year[25]) / 
        2 + 
        (auxiliary_table$yield_poplar_MS_taDM_ha_year[26] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[27] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[28] +
           auxiliary_table$yield_poplar_MS_taDM_ha_year[29] + 
           auxiliary_table$yield_poplar_MS_taDM_ha_year[30])
    }
    
  } # End for loop drought
  
  
  
  # Milk production output after drought effects no AF
  milk_t_ha_no_AF <- auxiliary_table$milk_t_ha_year_no_AF + milk_t_ha_rest_pot
  
  # Milk production output after drought effects AF
  milk_t_ha_AF <- auxiliary_table$milk_t_ha_year_AF + milk_t_ha_rest_pot
  
  # Poplar yield output after drought effects
  yield_poplar_MS_taDM_ha <-  auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year
  
  # Seed cost no AF after drought effects
  seed_costs_no_AF <-  auxiliary_table$seed_costs_year_no_AF
  
  # Seed cost AF after drought effects
  seed_costs_AF <-  auxiliary_table$seed_costs_year_AF
  
  # Production ####  
  
  ## Milk production
  produced_milk_t_no_AF <-  (milk_t_ha_no_AF - milk_reduction_t_ha_no_AF) * 
    production_area 
  
  produced_milk_t_AF <- (milk_t_ha_AF * 
                           grass_yield_fac_AF - 
                           milk_reduction_t_ha_AF
  ) *
    grass_area
  
  
  
  ## Tree production
  
  produced_poplar_MS <- yield_poplar_MS_taDM_ha * agroforestry_area
  
  # Costs ####
  
  ## Land lease costs - costs for leasing land or the opportunity costs if the land is owned 
  land_costs <- c(rep(land_lease_price_ha, n_years)) *  
    production_area
  
  ## Costs milk production ####
  
  ### Costs production area milk ####
  
  #### Fertilizer costs 
  fertelizer_costs_no_AF <- vv(fertilizer_costs_ha, var_CV, n_years) *
    production_area
  
  fertelizer_costs_AF <-  vv(fertilizer_costs_ha, var_CV, n_years) * 
    grass_area
  
  #### Pesticide costs 
  pesticide_costs_no_AF <-  vv(pesticide_cost_ha, var_CV, n_years) *
    production_area
  
  pesticide_costs_AF <- vv(pesticide_cost_ha, var_CV, n_years) *  # Influence missing, no data fond, experts don't think there is one 
    grass_area
  
  #### Other costs for fields
  other_costs_field <-  vv(other_costs_field_ha, var_CV, n_years) *
    production_area 
  
  
  #### Labor costs
  labor_costs_no_AF <-  vv(labor_costs_ha, var_CV, n_years) *   
    production_area
  
  labor_costs_AF <- (labor_costs_no_AF + 
                       labor_costs_no_AF * 
                       AF_on_labor_costs) *   
    grass_area 
  
  
  
  
  contractor_machinery_rent_costs <-  vv(contractor_machinery_rent_costs_ha, var_CV, n_years) *
    production_area
  
  fuels_lubricants_costs <- vv(fuels_lubricants_costs_ha, var_CV, n_years) *
    production_area
  
  machinery_upkeep_costs <-  vv(machinery_upkeep_costs_ha, var_CV, n_years) *
    production_area
  
  insurances_costs <- vv(insurances_costs_ha, var_CV, n_years) *
    production_area
  
  other_costs_operation <-  vv(other_costs_operation_ha, var_CV, n_years) *
    production_area
  
  depreciation <- vv(depreciation_ha, var_CV, n_years) *
    production_area
  
  
  
  ### Costs produced milk ####   potential milk production instead of produced_milk_t_AF produced_milk_t_no_AF##################################################################################################################################################
  
  veterinary_costs_no_AF <- (veterinary_costs_t_milk_pot + 
                               veterinary_costs_t_milk_heat_no_AF) * 
    milk_t_ha_pot * production_area 
  
  veterinary_costs_AF <-  (veterinary_costs_t_milk_pot + 
                             veterinary_costs_t_milk_heat_AF) * 
    milk_t_ha_pot * production_area
  
  insemination_costs <- vv(insemination_costs_t_milk, var_CV, n_years) *
    milk_t_ha_pot * production_area
  
  other_costs_milk <- vv(other_costs_milk_t_milk, var_CV, n_years) *
    milk_t_ha_pot * production_area
  
  animal_purchase_costs <-  vv(animal_purchase_costs_t_milk, var_CV, n_years) *
    milk_t_ha_pot * production_area
  
  feed_purchase_costs_no_AF <-  vv(feed_purchase_costs_t_milk, var_CV, n_years) *
    milk_t_ha_pot * production_area
  
  feed_purchase_costs_AF <- vv(feed_purchase_costs_t_milk, var_CV, n_years) *  
    milk_t_ha_pot * production_area
  
  energy_water_costs <- vv(energy_water_costs_t_milk, var_CV, n_years) *
    milk_t_ha_pot * production_area
  
  building_upkeep_costs <-  vv(building_upkeep_costs_t_milk, var_CV, n_years) *
    milk_t_ha_pot * production_area
  
  
  
  
  ### Adding up costs milk ####
  costs_milk_no_AF <- land_costs +
    seed_costs_no_AF +
    fertelizer_costs_no_AF +
    pesticide_costs_no_AF +
    other_costs_field +
    labor_costs_no_AF +
    contractor_machinery_rent_costs +
    fuels_lubricants_costs +
    machinery_upkeep_costs +
    insurances_costs +
    other_costs_operation +
    depreciation +
    veterinary_costs_no_AF +
    insemination_costs +
    other_costs_milk +
    animal_purchase_costs +
    feed_purchase_costs_no_AF +
    energy_water_costs +
    building_upkeep_costs
  
  
  costs_milk_AF <-  land_costs +
    seed_costs_AF +
    fertelizer_costs_AF +
    pesticide_costs_AF +
    other_costs_field +
    labor_costs_AF +
    contractor_machinery_rent_costs +
    fuels_lubricants_costs +
    machinery_upkeep_costs +
    insurances_costs +
    other_costs_operation +
    depreciation +
    veterinary_costs_AF +
    insemination_costs +
    other_costs_milk +
    animal_purchase_costs +
    feed_purchase_costs_AF +
    energy_water_costs +
    building_upkeep_costs
  
  
  
  
  ## Costs tree production ####
  
  ### Costs production area tree ####
  
  #### AF planning costs 
  planinng_costs <- c(vv(AF_planning_costs, var_CV, 1), rep(0, (n_years - 1))) + # Y1 
    c(vv(AF_area_on_AF_planning_costs, var_CV, 1), rep(0, (n_years - 1))) * 
    log(production_area) # production_area log() is fine minimum area is >= 5 so no issues with x <=1 
  
  #### Tillage costs before planting
  tillage_costs <-  c(vv(tillage_costs_ha, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  tillage_planting_preparation_costs <- c(vv(tillage_planting_preparation_costs_ha, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Weed control in 1st year
  weed_control_costs <- c(vv(weed_control_costs_ha, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Seed costs
  seedling_costs_popplar_10_20Y <-  c(vv(seedlings_ha_poplar_10_20Y, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area *
    c(vv(seedling_price_poplar_0.2m, var_CV, 1), rep(0, (n_years - 1)))
  
  #### Planting costs 
  planting_costs_mechanical <-  c(vv(planting_costs_ha_mechanical, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Fencing costs 
  fence_construction_costs <- c(vv(fence_construction_costs_ha, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Tree upkeep costs in 1st year
  tree_upkeep_costs_1stY <- c(vv(tree_upkeep_costs_ha_1stY, var_CV, 1), rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Tree upkeep costs after 1st year
  tree_upkeep_costs_after_1stY <- c(0, vv(tree_upkeep_costs_ha_after_1stY, var_CV, n_years - 1)) *
    agroforestry_area
  
  #### Harvest costs 
  cutting_costs <-  vv(cutting_costs_taDM, var_CV, n_years) * # Depending on the interval
    produced_poplar_MS
  
  chopping_costs <- vv(chopping_costs_taDM, var_CV, n_years) * # Depending on the interval
    produced_poplar_MS
  
  storage_costs_35perc <- vv(storage_costs_35perc_taDM, var_CV, n_years) * # Depending on the interval
    produced_poplar_MS
  
  #### Reconversion costs after the last year
  reconversion_cost <-  c(rep(0, n_years - 1), reconversion_cost_ha) * # Last year
    agroforestry_area
  
  
  ### Adding up costs tree ####
  
  costs_poplar_35perc <-  planinng_costs +
    tillage_costs +
    tillage_planting_preparation_costs +
    weed_control_costs +
    seedling_costs_popplar_10_20Y +
    planting_costs_mechanical +
    fence_construction_costs +
    tree_upkeep_costs_1stY +
    tree_upkeep_costs_after_1stY +
    cutting_costs+
    chopping_costs+
    storage_costs_35perc+
    reconversion_cost
  
  
  # Benefits ####  
  
  ## Benefits milk production ####
  
  ### Benefits production area ####
  basic_subsidies_benefits <- vv(basic_subsidies_ha, var_CV, n_years) *
    production_area
  
  
  ### Benefits produced milk ####
  milk_sales_benefits_no_AF <-  vv(milk_price_t, var_CV, n_years) *
    produced_milk_t_no_AF 
  
  milk_sales_benefits_AF <- vv(milk_price_t, var_CV, n_years) *
    produced_milk_t_AF
  
  other_animal_products_benefits <- vv(other_animal_products_t_milk, var_CV, n_years) *
    produced_milk_t_no_AF 
  
  
  
  
  
  
  ### Adding up benefits milk ####
  benefits_milk_no_AF <-  basic_subsidies_benefits +
    milk_sales_benefits_no_AF +
    other_animal_products_benefits
  
  benefits_milk_AF <- basic_subsidies_benefits +
    milk_sales_benefits_AF +
    other_animal_products_benefits
  
  
  
  ## Benefits tree production ####
  
  ### Benefits production area tree ####
  agroforestry_subsidies <- c(rep(agroforestry_subsidies_ha, n_years)) *
    agroforestry_area
  
  ### Benefits produced wood chips ####
  wood_chips_sales_benefits_35perc <- vv(wood_chips_price_t_35perc, var_CV, n_years) *
    (produced_poplar_MS / (1 - residual_moisture_35perc))
  
  ### Adding up benefits tree ####
  benefits_poplar_35perc <- agroforestry_subsidies +
    wood_chips_sales_benefits_35perc
  
  # Ecological Aspects #### 
  
  ## Erosion
  erosion_costs_no_AF <-  vv(EcoB_erosion, var_CV, n_years) * 
    production_area
  
  erosion_costs_AF <- erosion_costs_no_AF *
    (1 - vv(AF_on_erosion, var_CV, n_years))
  
  ## Ground water gain AF
  ground_water_gain_AF <- vv(EcoB_groundwater_AF, var_CV, n_years) *
    production_area
  
  ## Pollination services AF 
  pollination_AF <- vv(EcoB_pollination_AF, var_CV, n_years) *
    agroforestry_area
  
  ## Carbon storage 
  carbon_storage_AF <-  vv(EcoB_carbon_storage_AF, var_CV, n_years) *
    agroforestry_area
  
  ## Nutrient loss
  nutrient_loss_costs_no_AF <-  vv(EcoB_nutrient_loss, var_CV, n_years)
  
  nutrient_loss_costs_AF <- vv(EcoB_nutrient_loss, var_CV, n_years) *
    (1 - vv(AF_on_nutrient_loss, var_CV, n_years))
  
  ## Biodiversity and aesthetics 
  biodiversity_aesthetics_AF <- vv(EcoB_biodiversity_aesthetics, var_CV, n_years) *
    agroforestry_area
  
  ## Adding up ecological aspects 
  
  ecological_aspects_no_AF <- 0 -
    erosion_costs_no_AF -
    nutrient_loss_costs_no_AF
  
  ecological_aspects_AF <-  0 - 
    erosion_costs_AF + 
    ground_water_gain_AF +
    pollination_AF +
    carbon_storage_AF -
    nutrient_loss_costs_AF +
    biodiversity_aesthetics_AF
  
  
  
  # Profits ####
  
  ## Profits no agroforestry ####
  profit_no_AF <- benefits_milk_no_AF - costs_milk_no_AF
  
  profit_no_AF_final <- ifelse(ecological_aspects_used == "NO", 
                               profit_no_AF,
                               profit_no_AF + 
                                 ecological_aspects_no_AF
  )
  
  
  
  ## Profits agroforestry ####
  profit_AF_poplar_35perc <-  benefits_milk_AF - costs_milk_AF + benefits_poplar_35perc - costs_poplar_35perc
  
  profit_AF_final_poplar_35perc <- ifelse(ecological_aspects_used == "NO", 
                                          profit_AF_poplar_35perc,
                                          profit_AF_poplar_35perc + 
                                            ecological_aspects_AF
  )
  
  
  
  # Net present value (NPV) ####
  NPV_no_AF <-  discount(profit_no_AF_final, discount_rate, calculate_NPV = TRUE)
  
  NPV_AF_poplar_35perc <- discount(profit_AF_final_poplar_35perc, discount_rate, calculate_NPV = TRUE)
  
  
  
  # Return ####
  return(list(NPV_no_AF = NPV_no_AF,
              NPV_AF = NPV_AF_poplar_35perc,
              NPV_decision_do = NPV_AF_poplar_35perc - NPV_no_AF,
              Cashflow_no_AF = profit_no_AF_final,
              Cashflow_AF = profit_AF_final_poplar_35perc,
              Cashflow_decision_do = profit_AF_final_poplar_35perc - profit_no_AF_final))
  
  
  
}  # End function


# Monte Carlo simulation ####
dairy_mc_simulation <- mcSimulation(estimate = as.estimate(input_table),
                                    model_function = dairy_model,
                                    numberOfModelRuns = 10000,
                                    functionSyntax = "plainNames"
)

# Save results in form of "data/.simulation_results/dairy_mc_simulation_10k_004N_XXha"  for no ecological Aspects, area 5 to 50 ha
#                         "data/.simulation_results/dairy_mc_simulation_10k_004Y_XXha"  for yes ecological Aspects, area 5 to 50 ha
#                         "data/.simulation_results/dairy_mc_simulation_10k_004N_10ha"  for no ecological Aspects, area 10 ha
#                         "data/.simulation_results/dairy_mc_simulation_10k_004Y_10ha"  for yes ecological Aspects, area 10 ha
saveRDS(dairy_mc_simulation,"data/.simulation_results/dairy_mc_simulation_10k_004N_10ha")





# Other scripts #### 

#cmip6_temp_gen.R
#heat_day_extraction_Kall.R
#heat_day_extraction_Schneifel.R
#calculating_THI_range.R

#for_Calculating_EVPI_in_seperat_session.R
#for_Calculating_EVPI_in_seperat_session_2.R

#analysing_results_model_agroforestry_dairy-farm.R
#Plots.R

#impact_pathway_agroforestry_dairy-farm_005_german.R
#citations.R