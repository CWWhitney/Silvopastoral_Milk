library(decisionSupport)
library(ggplot2)
library(dplyr)

# Input ranges #######
input_table <- read.csv("input_table.csv") #, stringsAsFactors = FALSE)
# Setting seed 
#set.seed(52393)

## Testing input table in function steps 
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n) # https://github.com/CWWhitney/Decision_Analysis_Course
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}
generated_variables <- make_variables(as.estimate(input_table))
# End of input #####

# DA Model
silvopastoral <- function(x, varnames) {
  
  # Initialize vectors to store results
  drought_occurrence <- logical(num_years) # Track drought occurrences - TRUE or FALSE -> affects grassland
  heat_event_occurrence <- logical(num_years) # Track heat events occurrences - TRUE or FALSE -> affects animals
  
  # Treeless System ####
  seed_costs <- numeric(num_years)
  fertilizer_cost_ha <- numeric(num_years)
  pest_weed_cost_ha <- numeric(num_years)
  irrigation_costs <- numeric(num_years)
  other_costs_field_ha <- numeric(num_years)
  THI <- numeric(num_years)
  
  Labour_costs <- vv(labour_cost, var_CV, num_years) 
  # grassland cost equations
  seed_costs <- vv(seed_costs_ha, var_CV, num_years) * area
  fertilizer_cost <- vv(fertilizer_costs_ha, var_CV, num_years) * area
  pest_weed_cost <- vv(pest_weed_cost_ha, var_CV, num_years) * area
  labor_costs <-  vv(grassland_labour, var_CV, num_years) * area * Labour_costs
  machinery_costs <-  (vv(contractor_machinery_rent_costs_ha, var_CV, num_years) * area )+ 
    (vv(fuels_lubricants_costs_ha, var_CV, num_years) * area) +
    (vv(machinery_upkeep_costs_ha, var_CV, num_years) * area)
  grass_other_costs_field <- (vv(insurances_costs_ha, var_CV, num_years) * area) + 
    (vv(depreciation_ha, var_CV, num_years) * area) +(vv(other_costs_field_ha, var_CV, num_years) * area)
  
  # milk production cost equations
  num_cows <- as.integer(Livestock_density * area)
  cow_mortality_rate <- vv(mortality_rate, var_CV, num_years)
  num_cows <- as.integer(num_cows * (1-cow_mortality_rate/100))
  
  milk_per_cow <- vv(milk_production_kg_cow_day, var_CV, num_years) * (365 - vv(dry_days_per_cow, var_CV, num_years)) # account for dry days
  milk_production_t <- milk_per_cow * num_cows * 0.0011 # Kg to T
  
  water_costs <- vv(daily_water_per_cow, var_CV, num_years) * vv(cost_water_per_l, var_CV, num_years) * num_cows
  insemination_costs <- vv(insemination_costs_t_milk, var_CV, num_years) * milk_production_t
  veterinary_costs <- vv(veterinary_costs_t_milk, var_CV, num_years) * milk_production_t
  feed_costs <- vv(feed_costs_t, var_CV, num_years) * feed_kg_year *  num_cows
  cow_labour_cost <- vv(cow_labour, var_CV, num_years) * skill_labor_costs * num_cows
  animal_other_costs <- (vv(building_upkeep_costs, var_CV, num_years)) + 
    (vv(other_costs_milk_t_milk, var_CV, num_years) * milk_production_t)
  
  
  # Silvopastoral System ####
  # Initialize variables
  AF_planning_cost <- rep(0, num_years) # Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[€]
  AF_dig_plant_holes <- rep(0, num_years) # Second step of implementation: digging/drilling holes for the trees [€]
  AF_tree_cost <- rep(0, num_years) # Cost per tree sapling [€]
  AF_plant_tree_cost <- rep(0, num_years) # Labour cost for planting one tree [€] -
  AF_protect_cost <- rep(0, num_years) # Material cost of tree protection mesh [€]
  AF_weed_protect_cost <- rep(0, num_years) #Material cost of weed suppressing fleece [€]
  AF_compost_cost <- rep(0, num_years) # Cost of compost used during planting [€]
  AF_irrigation_planting_cost <- rep(0, num_years) # Cost for watering in newly planted trees [€]
  AF_total_planting_cost <- rep(0, num_years)
  AF_pruning <- rep(0, num_years) # Labour cost of pruning fruit trees [€]
  ES3_application <- rep(0, num_years) # Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  AF_annual_irrigation <- rep(0, num_years) # Cost of annual irrigation of tree rows [€]
  AF_timber_harvest_cost <-  rep(0, num_years) # Labour hours of mowing the tree rows manually [h/ha]
  ES3_subsidy <- rep(0, num_years)
  # area of grassland in silvopastoral system
  AF_arable_area <- area - (area * agroforestry_area_percentage)
  # grassland cost equations
  AF_seed_costs <- vv(seed_costs_ha, var_CV, num_years) * AF_arable_area
  AF_fertilizer_cost <- vv(fertilizer_costs_ha, var_CV, num_years) * AF_arable_area
  AF_pest_weed_cost <- vv(pest_weed_cost_ha, var_CV, num_years) * AF_arable_area
  AF_labor_costs <-  vv(labor_costs_ha, var_CV, num_years) * AF_arable_area
  AF_machinery_costs <-  (vv(contractor_machinery_rent_costs_ha, var_CV, num_years) * AF_arable_area) + 
    (vv(fuels_lubricants_costs_ha, var_CV, num_years) * AF_arable_area) +
    (vv(machinery_upkeep_costs_ha, var_CV, num_years) * AF_arable_area)
  AF_grass_other_costs_field <- (vv(insurances_costs_ha, var_CV, num_years) * AF_arable_area) + 
    (vv(depreciation_ha, var_CV, num_years) * AF_arable_area) + 
    (vv(other_costs_field_ha, var_CV, num_years) * AF_arable_area)
  
  # Tree cost equations
  num_trees <- (area * agroforestry_area_percentage) * tree_density 
  AF_planning_cost[1] <-    planning_consulting + farmer_planning_time * Labour_costs[1]
  AF_dig_plant_holes[1] <- dig_planting_holes * Labour_costs[1]
  AF_tree_cost[1] <- seedling_price_poplar * num_trees
  AF_plant_tree_cost[1] <- planting_trees * Labour_costs[1]
  AF_protect_cost[1] <- plant_protection * num_trees
  AF_weed_protect_cost[1] <- weed_protection * num_trees
  AF_compost_cost[1] <- compost_planting * compost_price * num_trees
  AF_irrigation_planting_cost[1] <- irrigation_planting * cost_water_per_l * num_trees
  AF_annual_irrigation[1:3] <- vv(irrigation_123, var_CV = var_cv, 3)
  AF_annual_irrigation[4:harvest_interval] <- vv(irrigation_annual, var_CV = var_cv, length(4:harvest_interval))
  AF_annual_irrigation_cost <- AF_annual_irrigation * cost_water_per_l
  ES3_application <- vv(es3_application, var_cv, num_years) * Labour_costs #application for Eco Scheme subsidy has to be repeated annually 
  
  # Establishment - will repeat the year after harvest
  for (tree_year in seq(1, num_years, by = harvest_interval+1)) {
  AF_planning_cost[tree_year] <-    planning_consulting + farmer_planning_time * Labour_costs[1]
  AF_dig_plant_holes[tree_year] <- dig_planting_holes * Labour_costs[tree_year]
  AF_tree_cost[tree_year] <- seedling_price_poplar * num_trees
  AF_plant_tree_cost[tree_year] <- planting_trees * Labour_costs[tree_year]
  AF_protect_cost[tree_year] <- plant_protection * num_trees
  AF_weed_protect_cost[tree_year] <- weed_protection * num_trees
  AF_compost_cost[tree_year] <- compost_planting * compost_price * num_trees
  AF_irrigation_planting_cost[tree_year] <- irrigation_planting * cost_water_per_l * num_trees
  }
 
  # Though pruning is dependent on the tree growth, avg cost over the years is presumed 
  AF_pruning <- vv(pruning_annual, var_CV = var_cv, num_years) * Labour_costs * num_trees
  
  # timber growth and carbon captured to calculate carbon credit
  AF_growth <- numeric(num_years)
  AF_agb <- numeric(num_years)
  AF_bgb <- numeric(num_years)
  AF_carbon_captured <- numeric(num_years)
  AF_carbon_payment <- numeric(num_years)
  AF_timber_yield <- numeric(num_years)
  AF_timber_value <- numeric(num_years)
  AF_height <- initial_height
  
  for (year in 1:num_years) {
    AF_growth[year] <- AF_height
    
    # Calculate biomass
    AF_agb[year] <- AF_height * agb_conversion
    AF_bgb[year] <- AF_agb[year] * bgb_ratio
    total_biomass <- AF_agb[year] + AF_bgb[year]
    
    # Calculate carbon captured
    AF_carbon_captured[year] <- (total_biomass * carbon_fraction / 1000 ) * tree_density # Convert to tons
    #AF_carbon_payment[year] <- AF_carbon_captured[year] * carbon_price
    
    # Timber yield calculation only at harvest
    if (year %% harvest_interval == 0) {
      AF_timber_yield[year] <- AF_height * timber_conversion
      #AF_timber_value[year] <- AF_timber_yield[year] * timber_price 
      AF_timber_harvest_cost[year] <-
        vv(timber_harvest_labour, var_CV = var_cv, length(harvest_interval:num_years)) * labour_cost
      AF_height <- initial_height  # reset after harvest
    } else {
      AF_timber_yield[year] <- 0
      AF_timber_value[year] <- 0
      AF_height <- AF_height + growth_rate
    }
  }
  
  # milk production costs
  AF_num_cows <- as.integer(AF_Livestock_density * AF_arable_area)
  AF_cow_mortality_rate <- cow_mortality_rate # use the same rate initially 
  AF_num_cows <- as.integer(num_cows * (1-AF_cow_mortality_rate/100))
  
  AF_milk_per_cow <- vv(milk_production_kg_cow_day, var_CV, num_years) * (365 - vv(dry_days_per_cow, var_CV, num_years)) # account for dry days
  AF_milk_production_t <- AF_milk_per_cow * AF_num_cows * 0.0011 # Kg to T
  
  AF_water_costs <- vv(AF_daily_water_per_cow, var_CV, num_years) * vv(cost_water_per_l, var_CV, num_years) * AF_num_cows
  AF_insemination_costs <- vv(insemination_costs_t_milk, var_CV, num_years) * AF_milk_production_t
  AF_veterinary_costs <- vv(veterinary_costs_t_milk, var_CV, num_years) * AF_milk_production_t
  AF_feed_costs <- vv(feed_costs_t, var_CV, num_years) * AF_feed_kg_year *  AF_num_cows # feed cost is reduced as tree cuttings and leaves can be fed to  cattle
  AF_cow_labour_cost <- vv(cow_labour, var_CV, num_years) * skill_labor_costs * AF_num_cows
  
  # Introduce Effects ####
  # Impact drought and heat events on two systems using Conditional probability table
  # Initialize
  Reduction_Min <- numeric(num_years)
  Reduction_Max <- numeric(num_years)
  AF_Reduction_Min <- numeric(num_years)
  AF_Reduction_Max <- numeric(num_years)
  Yield_Per_Cow_Min <- numeric(num_years)
  Yield_Per_Cow_Max <- numeric(num_years)
  AF_Yield_Per_Cow_Min <- numeric(num_years)
  AF_Yield_Per_Cow_Max <- numeric(num_years)
  impact_prob <- numeric(num_years)  # storage for each year's impact probability
  
  # generate impact table based on CPT for drought and heat events
  CPT_impact <- make_CPT(
    parent_effects = list(c(0.5, -1), c(0.2, -0.5)),  # Drought has a stronger negative effect (-1), 
    # heat event has a moderate negative effect (-0.5)
    parent_weights = c(3, 1),  # Drought has a bigger influence (3), heat event less (1)
    b = 1.5,  # Non-linearity scaling factor
    child_prior = c(0.1, 0.9),  # Now P(Impact) is 40%, and P(No_Impact) is 60%
    child_states = c("Impact", "No_Impact"),  # Ensuring binary outcomes
    parent_names = c("Drought_event", "Heat_event"),  
    parent_states = list(c("No_drought", "Drought"), c("No_heat", "Heat"))  # Corrected parent_states
  )
  
  # check CPT_impact$child_states is NULL
  if (!is.null(CPT_impact$child_states) && 
      length(CPT_impact$child_states) > 0 &&
      length(CPT_impact$child_states) == length(CPT_impact$probs) &&
      all(!is.na(CPT_impact$probs)) &&
      sum(CPT_impact$probs) > 0) {
    
    impact_category <- sample(CPT_impact$child_states, 1, prob = CPT_impact$probs)
  } else {
    warning("Invalid CPT_impact input; defaulting to 'No_Impact'")
    impact_category <- "No_Impact"
  }
  
  # evaluate the effects on both system in a year
  for (year in 1:num_years) {
    drought_occurrence[year] <- chance_event(p_drought, n = 1)
    heat_event_occurrence[year] <- chance_event(p_heat, n = 1)
    
    # Determine CPT column name and assign multipliers based on event combinations
    if (!drought_occurrence[year] && !heat_event_occurrence[year]) {
      col_name <- "col_1"
      # No impact expected — all multipliers remain at baseline
      drought_multiplier <- 1.0
      heat_multiplier <- 1.0
      AF_drought_multiplier <- 1.0
      AF_heat_multiplier <- 1.0
      
    } else if (!drought_occurrence[year] && heat_event_occurrence[year]) {
      col_name <- "col_2"
      # Only heat event — adjust heat-related multipliers
      drought_multiplier <- 1.0
      heat_multiplier <- vv(heat_multiplier, var_CV, num_years)
      AF_drought_multiplier <- 1.0
      AF_heat_multiplier <- vv(af_heat_multiplier, var_CV, num_years)
      
    } else if (drought_occurrence[year] && !heat_event_occurrence[year]) {
      col_name <- "col_3"
      # Only drought event — adjust drought-related multipliers
      drought_multiplier <- vv(drought_multiplier, var_CV, num_years)
      heat_multiplier <- 1.0
      AF_drought_multiplier <- vv(af_drought_multiplier, var_CV, num_years)
      AF_heat_multiplier <- 1.0
      
    } else {
      col_name <- "col_4"
      # Both drought and heat — adjust all multipliers
      drought_multiplier <- vv(drought_multiplier, var_CV, num_years)
      heat_multiplier <- vv(heat_multiplier, var_CV, num_years)
      AF_drought_multiplier <- vv(af_drought_multiplier, var_CV, num_years)
      AF_heat_multiplier <- vv(af_heat_multiplier, var_CV, num_years)
    }
    
    # Get impact probability from CPT for this year's conditions
    impact_prob[year] <- CPT_impact$CPT[["Impact", col_name]]

    # Adjust grassland costs due to drought
    if (drought_occurrence[year]) {
      # Treeless Sytem
      fertilizer_cost[year] <- fertilizer_cost[year] * (1 + drought_multiplier[year] *
                                                          impact_prob[year])
      pest_weed_cost[year] <- pest_weed_cost[year] * (1 + drought_multiplier [year] * 
                                                        impact_prob[year])
      other_costs_field_ha[year] <- other_costs_field_ha[year] * 
        (1 + drought_multiplier [year] * impact_prob[year])
      irrigation_costs[year] <- ifelse(drought_occurrence[year], 
        (vv(cost_water_per_l, var_CV, num_years) * grass_water * area), 0) # multiplier not used here as it is an extra cost in case of event
      
      #Silvopastoral System (mitigated effects)
      AF_fertilizer_cost[year] <- fertilizer_cost[year] * (1 +  AF_drought_multiplier[year] * 
                                                             impact_prob[year])
      AF_pest_weed_cost[year] <- pest_weed_cost[year] * (1 + AF_drought_multiplier [year] * 
                                                           impact_prob[year])
      AF_annual_irrigation_cost[year] <- ifelse(drought_occurrence[year], 
        (cost_water_per_l * AF_annual_irrigation) * (1 +  AF_drought_multiplier[year] * 
        impact_prob[year]), (AF_annual_irrigation * cost_water_per_l)) 
    }
    
    # Adjust animal-related costs due to heat event
    if (heat_event_occurrence[year]) {
      cow_mortality_rate[year] <- heat_mortality_rate 
      water_costs[year] <- water_costs[year] * (1 + heat_multiplier[year] *impact_prob[year])
      veterinary_costs[year] <- veterinary_costs[year] * (1 + heat_multiplier[year] * impact_prob[year])
      num_cows[year] <- as.integer(num_cows * (1 - cow_mortality_rate[year] / 100))
      
      # Silvopastoral System (mitigated effects)
      AF_cow_mortality_rate[year] <- AF_heat_mortality_rate 
      AF_water_costs[year] <- water_costs[year] * (1 + AF_heat_multiplier[year] * impact_prob[year])
      AF_veterinary_costs[year] <- veterinary_costs[year] * (1 + AF_heat_multiplier[year] * impact_prob[year])
      AF_num_cows[year] <- as.integer(num_cows * (1 - AF_cow_mortality_rate [year]/ 100))
      
      # Calculate THI effect (on animal welfare and therefore) on milk production 
      air_temp <- vv(air_temperature, var_CV, num_years)
      humidity <- vv(relative_humidity, var_CV, num_years)
      #Calculate THI using the formula: National Research Council (1971)
      THI[year] <- (1.8 * air_temp[year]) + 32 - ((0.55 - 0.0055 * humidity[year]) * 
                                                    ((1.8 * air_temp[year]) - 26))

      # Apply milk production reduction only if THI > 68
      if (THI[year] > 68) {
        Reduction_Min[year] <- (THI[year] - 67) * reduction_min
        Reduction_Max[year] <- (THI[year] - 67) * reduction_max
        Yield_Per_Cow_Min[year] <- milk_per_cow - Reduction_Min[year]
        Yield_Per_Cow_Max[year] <- milk_per_cow - Reduction_Max[year]
        
        selected_yield <- runif(1, Yield_Per_Cow_Min[year], Yield_Per_Cow_Max[year])
        heat_milk_production_t <- selected_yield * num_cows * 0.0011
        milk_production_t[year] <- heat_milk_production_t
        
        # Silvopastoral System (less impact due to trees reducing heat stress)
        AF_Reduction_Min[year] <- (THI[year] - 67) * AF_reduction_min
        AF_Reduction_Max[year] <- (THI[year] - 67) * AF_reduction_max
        AF_Yield_Per_Cow_Min[year] <- AF_milk_per_cow - AF_Reduction_Min[year]
        AF_Yield_Per_Cow_Max[year] <- AF_milk_per_cow - AF_Reduction_Max[year]
        
        AF_selected_yield <- runif(1, AF_Yield_Per_Cow_Min[year], AF_Yield_Per_Cow_Max[year])
        AF_heat_milk_production_t <- AF_selected_yield * AF_num_cows * 0.0011
        AF_milk_production_t[year] <- AF_heat_milk_production_t
      } else {
        THI[year] <- 0
        #milk_production_t[year] <- 0
        #AF_milk_production_t[year] <- 0
      }
    }
  }

  # Treeless System bottom line ####
  # Costs
  treeless_grass_expense <- seed_costs + fertilizer_cost + pest_weed_cost + 
    irrigation_costs  +labor_costs + machinery_costs + grass_other_costs_field
  
  treeless_animal_expense <-  cow_labour_cost + water_costs + veterinary_costs + 
    feed_costs + animal_other_costs
  
  treeless_total_expense <- treeless_grass_expense + treeless_animal_expense
  # Benefits
  # grassland benefit
  treeless_basic_subsidies_benefits <- vv(basic_subsidies_ha, var_CV, num_years) * area
  
  # milk production benefit
  treeless_milk_sales_benefits <-  vv(milk_price_t, var_CV, num_years) * milk_production_t
  treeless_other_animal_products_benefits <- vv(other_animal_products_t_milk, var_CV, num_years) * 
    milk_production_t
  
  treeless_total_benefit <- treeless_basic_subsidies_benefits + treeless_milk_sales_benefits 
    + treeless_other_animal_products_benefits
  # Bottom Line
  treeless_revenue <- treeless_total_benefit - treeless_total_expense
  
  # Silvopastoral System bottom line ####
  AF_grass_expense <- AF_seed_costs + AF_fertilizer_cost + AF_pest_weed_cost + 
    AF_labor_costs + AF_machinery_costs #+AF_irrigation_costs
  
  AF_total_planting_cost <- AF_dig_plant_holes + AF_tree_cost + AF_plant_tree_cost + 
    AF_protect_cost + AF_weed_protect_cost + AF_compost_cost + #AF_irrigation_system_cost + 
    + AF_irrigation_planting_cost
  AF_total_investment_cost <- AF_planning_cost + AF_total_planting_cost #Investment cost of AF system implementation
  AF_total_treerow_management_cost <- ES3_application + AF_pruning + # AF_root_pruning + + AF_mowing_treerow
    AF_annual_irrigation_cost + AF_timber_harvest_cost
  AF_tree_expense <- AF_total_treerow_management_cost + AF_total_investment_cost + 
    AF_total_planting_cost
  
  AF_animal_expense <-  AF_cow_labour_cost + AF_water_costs + AF_veterinary_costs + 
    AF_feed_costs + animal_other_costs
  
  AF_total_expense <- AF_grass_expense + AF_animal_expense + AF_tree_expense
  
  # Benefits
  # grassland benefits
  basic_subsidies_benefits <- vv(basic_subsidies_ha, var_CV, num_years) * AF_arable_area
  ES3_subsidy[1:num_years] <- es3_subsidy * (area * agroforestry_area_percentage)
  
  # tree benefits
  AF_timber_sale <- AF_timber_yield * timber_price * tree_density
  # Or also sold as wood chips
  #wood_chips_sales_benefits_35perc <- vv(wood_chips_price_t_35perc, var_CV, num_years) *
  #((AF_timber_yield * tree_density) / (1 - residual_moisture_35perc))
  
  # Ecological benefits from trees#### 
  # Erosion take into account density
  erosion_costs_AF <- vv(EcoB_erosion, var_CV, num_years) * area * 
    (1 - vv(AF_on_erosion, var_CV, num_years))
  ## Ground water gain AF
  ground_water_gain_AF <- vv(EcoB_groundwater_AF, var_CV, num_years) * 
    (area * agroforestry_area_percentage)
  ## Pollination services AF 
  pollination_AF <- vv(EcoB_pollination_AF, var_CV, num_years) * 
    (area * agroforestry_area_percentage)
  ## Carbon credit payment 
  AF_carbon_payment <- AF_carbon_captured * carbon_price 
  ## Nutrient loss
  nutrient_loss_costs_AF <- vv(EcoB_nutrient_loss, var_CV, num_years) * 
    (1 - vv(AF_on_nutrient_loss, var_CV, num_years)) * (area * agroforestry_area_percentage)
  ## Biodiversity and aesthetics 
  biodiversity_aesthetics_AF <- vv(EcoB_biodiversity_aesthetics, var_CV, num_years) * 
    (area * agroforestry_area_percentage)
  
  # milk production benefits
  AF_milk_sales_benefits <-  vv(milk_price_t, var_CV, num_years) * AF_milk_production_t
  # in case of diversification
  # AF_other_animal_products_benefits <- vv(AF_other_animal_products_t_milk, var_CV, num_years) *
      # milk_for_other_products
  
  AF_total_benefit <- basic_subsidies_benefits + ES3_subsidy + AF_timber_sale + 
    AF_milk_sales_benefits + erosion_costs_AF + ground_water_gain_AF + 
    pollination_AF + AF_carbon_payment + nutrient_loss_costs_AF + biodiversity_aesthetics_AF
  #+ AF_other_animal_products_benefits
  # Bottom Line
  AF_revenue <- AF_total_benefit - AF_total_expense
  
  #NPV calculation ####
  AF_NPV <- discount(AF_revenue, discount_rate=discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow <- discount(AF_revenue, discount_rate=discount_rate,
                           calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  #grassland system  
  Treeless_NPV <- discount(treeless_revenue, discount_rate = discount_rate,
                           calculate_NPV = TRUE) #NVP of grassland
  Treeless_cash_flow <- discount(treeless_revenue, discount_rate = discount_rate,
                                 calculate_NPV = FALSE) #Cash flow of grassland
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of grassland
  
  Decision_revenue <- AF_revenue - treeless_revenue
  Decision_NPV <- discount(Decision_revenue, discount_rate = discount_rate,
                           calculate_NPV = TRUE) 
  
  return(list(NPV_Agroforestry_System = AF_NPV,
              NPV_Treeless_System = Treeless_NPV,
              NPV_decision = Decision_NPV,
              AF_Cashflow = AF_cash_flow,
              Treeless_Cashflow = Treeless_cash_flow,
              AF_CumulativeCashflow = AF_cum_cash_flow,
              Treeless_CumulativeCashflow = Treeless_cum_cash_flow 
  ))
}

#Run the Monte Carlo analysis of the model from decisionSupport
mcSimulation_results <- mcSimulation(
  estimate = estimate_read_csv(fileName = "input_table.csv"),
  model_function = silvopastoral,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames")

# Print the output using make_variables function
#print(generated_variables)
write.csv(generated_variables, file = "variables_output.csv", row.names = FALSE)

# PLOTS####
# plot NPV distributions
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7,
                   x_axis_name = "Outcome as NPV in € for the given area",
                   scale_x_continuous(labels = function(x) x / 100000),
                   ggtitle("Net Present Value of the system with intangibles"),
                   legend.position="bottom")
ggsave(
  filename = "images/NPV_grassland_vs_silvopastoral.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

# Create timber and carbon credit data frame for visualization
poplar_growth <- data.frame(
  Year = 1:num_years,
  Height = AF_growth,
  AGB = AF_agb,
  BGB = AF_bgb,
  Carbon_Captured = AF_carbon_captured,
  Carbon_Payment = AF_carbon_payment,
  Timber_Yield_m3 = AF_timber_yield,
  Timber_Value_EUR = AF_timber_sale
)

ggplot(poplar_growth, aes(x = Year)) +
  geom_line(aes(y = Carbon_Payment), color = "blue", size = 1) +
  geom_point(aes(y = Carbon_Payment), color = "blue") +
  labs(title = "Annual Carbon Payments in Agroforestry",
       x = "Year",
       y = "Payment (EUR)",
       caption = "Blue: Carbon") +
  theme_minimal()

ggsave(
  filename = "images/Carbon_Payments.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
ggplot(poplar_growth, aes(x = Year)) +
  geom_line(aes(y = Timber_Value_EUR), color = "forestgreen", size = 1) +
  geom_point(aes(y = Timber_Value_EUR), color = "forestgreen") +
  labs(title = "Annual Timber sale Payments in Agroforestry",
       x = "Year",
       y = "Payment (EUR)",
       caption = "Green: Timber") +
  theme_minimal()
ggsave(
  filename = "images/Timber_Sale.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

#combined graph
# ggplot(poplar_growth, aes(x = Year)) +
#   geom_line(aes(y = Carbon_Payment), color = "blue", size = 1) +
#   geom_point(aes(y = Carbon_Payment), color = "blue") +
#   geom_line(aes(y = Timber_Value_EUR), color = "forestgreen", size = 1) +
#   geom_point(aes(y = Timber_Value_EUR), color = "forestgreen") +
#   labs(title = "Annual Carbon and Timber Payments in Agroforestry",
#        x = "Year",
#        y = "Payment (EUR)",
#        caption = "Blue: Carbon | Green: Timber") +
#   theme_minimal()