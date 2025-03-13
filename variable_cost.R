### A hypothetical silvopastoral system with poplar SRC in Eifel that focuses on dairy production

#### Install Libraries ####
if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)

if (!requireNamespace("decisionSupport", quietly = TRUE)) {
  install.packages("decisionSupport")
}
library(decisionSupport)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("ggridges", quietly = TRUE)) {
  install.packages("ggridges")
}
library(ggridges)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)
# End of Libraries #####


library(decisionSupport)
library(ggplot2)
library(dplyr)
# Input table #######
input_table <- read.csv("input_table.csv") #, stringsAsFactors = FALSE)
# THI Calculation
#Tmax <- c(25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36) # in °C
#RH <- c(25, 30, 35, 40, 45, 50, 55, 60) # in %
# Create a dataframe of all combinations
#combinations <- expand.grid(Tmax = Tmax, RH = RH)
# Calculate THI using the formula: National Research Council (1971)
#combinations$THI_comb <- with(combinations, ((1.8 * Tmax + 32) - ((0.55 - 0.0055*RH) * (1.8*Tmax - 26))))
# Filter out rows where THI is less than 68
#filtered_combinations <- subset(combinations, THI >= 68)
# Setting seed 
#set.seed(52393)

## Testing input table in function steps 
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n) # https://github.com/CWWhitney/Decision_Analysis_Course
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}
generated_variables <- make_variables(as.estimate(input_table)) #Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part

# End of input #####

# DA Model
silvopastoral <- function(x, varnames) {

# Initialize vectors to store results
drought_occurrence <- logical(num_years)  # Track drought occurrences
seed_costs <- numeric(num_years)
fertilizer_cost_ha <- numeric(num_years)
pest_weed_cost_ha <- numeric(num_years)
irrigation_costs <- numeric(num_years)
other_costs_field_ha <- numeric(num_years)
THI <- numeric(num_years)

# grassland costs
seed_costs <- vv(seed_costs_ha, var_CV, num_years)
fertilizer_cost <- vv(fertilizer_costs_ha, var_CV, num_years) * area
pest_weed_cost <- vv(pest_weed_cost_ha, var_CV, num_years) * area
other_costs_field <- vv(other_costs_field_ha, var_CV, num_years) * area
labor_costs <-  vv(labor_costs_ha, var_CV, num_years) * area
machinery_costs <-  (vv(contractor_machinery_rent_costs_ha, var_CV, num_years) * area )+ 
  (vv(fuels_lubricants_costs_ha, var_CV, num_years) * area) +
  (vv(machinery_upkeep_costs_ha, var_CV, num_years) * area)
grass_other_costs_field <- (vv(insurances_costs_ha, var_CV, num_years) * area) + 
  (vv(depreciation_ha, var_CV, num_years) * area)

# milk production costs
num_cows <- as.integer(Livestock_density * area)
#num_cows <- Num_cows
cow_mortality_rate <- vv(mortality_rate, var_CV, num_years)
num_cows <- as.integer(num_cows * (1-cow_mortality_rate/100))
milk_per_cow <- vv(milk_production_kg_cow_day, var_CV, num_years) * (365 - vv(dry_days_per_cow, var_CV, num_years)) # account for dry days
milk_production_t <- milk_per_cow * num_cows * 0.0011 # Kg to T
water_costs <- vv(daily_water_per_cow, var_CV, num_years) * vv(cost_water_per_l, var_CV, num_years) * num_cows
insemination_costs <- vv(insemination_costs_t_milk, var_CV, num_years) * milk_production_t
veterinary_costs <- vv(veterinary_costs_t_milk, var_CV, num_years) * milk_production_t
feed_costs <- vv(feed_costs_t, var_CV, num_years) * feed_kg_year *  num_cows
cow_labour_cost <- vv(cow_labour, var_CV, num_years) * labor_costs_ha
animal_other_costs <- (vv(building_upkeep_costs, var_CV, num_years)) + (vv(other_costs_milk_t_milk, var_CV, num_years) * milk_production_t)

# Account for drought and heat stress years based on probability and compute its effect on system variables
for (year in 1:num_years) {
  # Determine if drought occurs this year
  drought_occurrence[year] <- chance_event(p_drought, n = 1)

  if (drought_occurrence[year]) {
    fertilizer_cost[year] <- fertilizer_cost[year] * drought_multiplier
    pest_weed_cost[year] <- pest_weed_cost[year] * drought_multiplier
    other_costs_field[year] <- other_costs_field[year] * drought_multiplier
    # Irrigation cost is applied only if drought occurs in that year, otherwise it's 0
    irrigation_costs[year] <- ifelse(drought_occurrence[year], (vv(cost_water_per_l, var_CV, num_years) * grass_water * area ), 0)
    # adjust cow mortality
    cow_mortality_rate[year] <- d_mortality_rate 
    water_costs[year] <- water_costs[year] * drought_multiplier
    veterinary_costs[year] <- veterinary_costs[year] * drought_multiplier
  }
  # Update cows
  num_cows[year] <- as.integer(num_cows * (1-cow_mortality_rate/100))
  
  # Calculate THI only if drought occurs
  if (drought_occurrence[year]) {
    air_temp <- vv(air_temperature, var_CV, num_years)  # Air temperature in °C
    humidity <- vv(relative_humidity, var_CV, num_years)  # Relative humidity in %
    THI[year] <- (1.8 * air_temp[year]) + 32 - ((0.55 - 0.0055 * humidity[year]) * ((1.8 * air_temp[year]) - 26))
    
    # Default values
    Reduction_Min[year] <- 0
    Reduction_Max[year] <- 0
    
    # Calculate reduction only when THI > 68
    if (THI[year] > 68) {
      Reduction_Min[year] <- (THI[year] - 67) * reduction_min
      Reduction_Max[year] <- (THI[year] - 67) * reduction_max
    } 
    
    # Calculate yield and milk production
    Yield_Per_Cow_Min[year] <- milk_per_cow - Reduction_Min[year]
    Yield_Per_Cow_Max[year] <- milk_per_cow - Reduction_Max[year]
    
    selected_yield <- runif(1, Yield_Per_Cow_Min[year], Yield_Per_Cow_Max[year])
    d_milk_production_t <- selected_yield * num_cows * 0.0011  # Kg to T
    milk_production_t[year] <- d_milk_production_t
  } else {
    THI[year] <- 0  # No THI calculation when no drought
    milk_production_t[year] <- 0  # Ensure indexing consistency
  }
  # 

  
  # Introduce increase in seed costs, feed price next year if drought occurs
  if (year > 1 && drought_occurrence[year - 1]) {
    seed_costs[year] <- seed_costs[year] * drought_multiplier
    feed_costs[year] <- feed_costs[year] * drought_multiplier
  }
  
  }

grass_expense <- seed_costs + fertilizer_cost + pest_weed_cost + irrigation_costs + other_costs_field +labor_costs + machinery_costs
animal_expense <-  cow_labour_cost + water_costs + veterinary_costs + feed_costs + animal_other_costs

total_expense <- grass_expense + animal_expense

# grassland benefit
basic_subsidies_benefits <- vv(basic_subsidies_ha, var_CV, num_years) * area
  
# milk production benefit
milk_sales_benefits <-  vv(milk_price_t, var_CV, num_years) * milk_production_t
other_animal_products_benefits <- vv(other_animal_products_t_milk, var_CV, num_years) * milk_production_t

total_benefit <- basic_subsidies_benefits + milk_sales_benefits + other_animal_products_benefits
# Bottom Line
revenue <- total_benefit - total_expense
# Intervention: Silvopastoral system

}

