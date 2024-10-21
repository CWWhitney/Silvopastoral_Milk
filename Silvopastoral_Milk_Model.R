

# Packages ####

# library(chillR)
 library(decisionSupport)
# library(DiagrammeR)
# library(DiagrammeRsvg)
# library(ggplot2)
# library(rsvg)
 library(tidyverse)


# Input table ####

input_table <- read.csv("data/inputs.csv", stringsAsFactors = FALSE)

# Setting seed ####

# set.seed(52393) # reason for number: Postleitzahl zu Hause

## Testing input table in function steps ####
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n) # https://github.com/CWWhitney/Decision_Analysis_Course
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}

make_variables(as.estimate(input_table))

# Model ####

dairy_model <- function(x, varnames) {
  
  # Creating the basic production systems for each run ####
  
  ## Production area
  production_area <-  #c(rep(vv(area, var_CV, 1), n_years)) 
    # production area has by far the most influence. 
    # Making it constant allows to compare other variables. #####
  production_area
  
  ## AF area
  AF_area_perc <- vv(agroforestry_area_percentage, var_CV, 1)
  
  agroforestry_area <-  AF_area_perc *
    production_area
  
  # Grassland yield factor with AF
  grass_yield_fac_AF <- vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_milk_production_AF_5_10_fac,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_milk_production_AF_10_15_fac,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_milk_production_AF_15_20_fac,
          AF_on_milk_production_AF_20_25_fac
        )
      )
    ),
    var_CV,
    n_years
  )
  
  ## Grass area
  grass_area <- production_area -
    agroforestry_area
  
  ## Cows per hectare potential
  cows_ha_pot <-  vv(cows_ha, var_CV, n_years)
  
  cows_ha_no_AF <- cows_ha_pot
  
  cows_ha_AF <- cows_ha_pot * grass_yield_fac_AF
  
  ## Milk per hectare potential as rep() so the potential of the herd is the same for the years in a run
  milk_t_cow <-  c(rep(vv(milk_production_t_cow_a, var_CV, 1), n_years))
  
  milk_t_ha_pot <- milk_t_cow * cows_ha_pot
  
  #milk_t_ha_pot_no_AF <- milk_t_cow * cows_ha_no_AF
  #milk_t_ha_pot_AF <- milk_t_cow * cows_ha_AF
  #milk_t_pot <- milk_t_ha_pot * production_area
  #milk_t_pot_no_AF <- milk_t_pot
  #milk_t_pot_AF <- milk_t_ha_pot * grass_area
  
  
  milk_t_ha_pot_loss_comp <- milk_t_ha_pot * (1 + vv(milk_loss_incl_production_perc, var_CV, n_years)) # The data of milk yield, is current production data, losses modeled included
  
  
  ## Milk production from grazing
  milk_grazing_perc <- vv(milk_production_grazing_perc, var_CV, 1)
  
  milk_t_ha_grazing_pot <-  milk_t_ha_pot_loss_comp *
    milk_grazing_perc
  
  ## Milk production rest
  milk_t_ha_rest_pot <- milk_t_ha_pot_loss_comp -
    milk_t_ha_grazing_pot
  
  ## Poplar per hectare potential
  yield_poplar_MS_taDM_ha_pot <-  vv(yield_poplar_MS_taDM_ha_a, var_CV, n_years) # Using medium sight conditions, moderate or bad soil condition but moderate or much rain, long periods with no rain have a great effect
  
  # Heat stress + AF on heat stress Tables ####
  
  source("functions/create_heat_stress_tables.R")
  create_heat_stress_tables_result <- create_heat_stress_tables(var_CV = var_CV, n_years = n_years)
  
  heat_table_1 <- create_heat_stress_tables_result$heat_table_1
  heat_table_no_AF_1 <- create_heat_stress_tables_result$heat_table_no_AF_1
  
  ## Heat stress days AF Tables ####
  
  source("functions/create_af_heat_stress_tables.R")
  create_af_heat_stress_tables_result <- create_af_heat_stress_tables(var_CV = var_CV, n_years = n_years)
  
  heat_table_AF <- create_af_heat_stress_tables_result$heat_table_AF
  heat_table_no_AF <- create_af_heat_stress_tables_result$heat_table_no_AF
  
  ## Thermal Heat Index (THI) Tables ####
  
  source("functions/create_THI_tables.R")
  create_THI_tables_result <- create_THI_tables(var_CV = var_CV, n_years = n_years)
  
  THI_table_stress_sum_no_AF <- create_THI_tables_result$THI_table_stress_sum_no_AF
  THI_table_stress_sum_AF <- create_THI_tables_result$THI_table_stress_sum_AF
  
  ### Milk reduction Thermal Heat Index (THI)
  
  milk_reduction_THI_kg_cow_day <- vv(milk_reduction_heat_THI_kg_cow_day, var_CV, n_years)
  
  milk_reduction_stress_kg_cow_no_AF <- THI_table_stress_sum_no_AF * milk_reduction_THI_kg_cow_day
  milk_reduction_stress_kg_cow_AF <- THI_table_stress_sum_AF * milk_reduction_THI_kg_cow_day
  
  milk_reduction_kg_cow_sum_no_AF <- rowSums(milk_reduction_stress_kg_cow_no_AF)
  milk_reduction_kg_cow_sum_AF <- rowSums(milk_reduction_stress_kg_cow_AF)
  
  milk_reduction_t_ha_no_AF <- (milk_reduction_kg_cow_sum_no_AF * cows_ha_no_AF) / 1000
  milk_reduction_t_ha_AF <- (milk_reduction_kg_cow_sum_AF * cows_ha_no_AF) / 1000
  
  # Heat stress days on veterinary costs ####
  increased_veterinary_costs_heat_day_fac <-  cbind(
    vv(increased_veterinary_costs_heat_25GC_perc, var_CV, n_years),
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
  
  veterinary_costs_t_milk_heat_no_AF <- rowSums(
    heat_table_no_AF *
      veterinary_costs_t_milk_pot_day *
      increased_veterinary_costs_heat_day_fac
  )
  
  veterinary_costs_t_milk_heat_AF <-  rowSums(
    heat_table_AF *
      veterinary_costs_t_milk_pot_day *
      increased_veterinary_costs_heat_day_fac
  )
  
  source("functions/create_drought_effects_tables.R")
  # directly assign the output of the function 
  milk_t_ha_no_AF <- create_drought_effects_tables(var_CV = var_CV, n_years = n_years)$milk_t_ha_no_AF
  milk_t_ha_AF <- create_drought_effects_tables(var_CV = var_CV, n_years = n_years)$milk_t_ha_AF
  yield_poplar_MS_taDM_ha <- create_drought_effects_tables(var_CV = var_CV, n_years = n_years)$yield_poplar_MS_taDM_ha
  seed_costs_no_AF <- create_drought_effects_tables(var_CV = var_CV, n_years = n_years)$seed_costs_no_AF
  seed_costs_AF <- create_drought_effects_tables(var_CV = var_CV, n_years = n_years)$seed_costs_AF

  
  # Production ####
  
  ## Milk production
  produced_milk_t_no_AF <-  (milk_t_ha_no_AF - milk_reduction_t_ha_no_AF) *
    production_area
  
  produced_milk_t_AF <- (milk_t_ha_AF *
                           grass_yield_fac_AF -
                           milk_reduction_t_ha_AF) *
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
  fertilizer_costs_no_AF <- vv(fertilizer_costs_ha, var_CV, n_years) *
    production_area
  
  fertilizer_costs_AF <-  vv(fertilizer_costs_ha, var_CV, n_years) *
    grass_area
  
  #### Pesticide costs
  pesticide_costs_no_AF <-  vv(pesticide_cost_ha, var_CV, n_years) *
    production_area
  
  pesticide_costs_AF <- vv(pesticide_cost_ha, var_CV, n_years) *  
    # Influence missing, no data found, experts don't think there is one
    grass_area
  
  #### Other costs for fields
  other_costs_field <-  vv(other_costs_field_ha, var_CV, n_years) *
    production_area
  
  #### Labor costs on grass
  labor_costs_no_AF <-  vv(labor_costs_ha, var_CV, n_years) *
    production_area
  # less grass to work on
  labor_costs_AF <- (labor_costs_no_AF) * (production_area - grass_area)

  contractor_machinery_rent_costs <-  vv(contractor_machinery_rent_costs_ha, 
                                         var_CV, n_years) *
    production_area
  
  fuels_lubricants_costs <- vv(fuels_lubricants_costs_ha, 
                               var_CV, n_years) *
    production_area
  
  machinery_upkeep_costs <-  vv(machinery_upkeep_costs_ha, 
                                var_CV, n_years) *
    production_area
  
  insurances_costs <- vv(insurances_costs_ha, 
                         var_CV, n_years) *
    production_area
  
  other_costs_operation <-  vv(other_costs_operation_ha, 
                               var_CV, n_years) *
    production_area
  
  depreciation <- vv(depreciation_ha, var_CV, n_years) *
    production_area
  
  ### Costs produced milk ####   
  # potential milk production instead of produced_milk_t_AF produced_milk_t_no_AF####
  
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
    fertilizer_costs_no_AF +
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
    fertilizer_costs_AF +
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
  planning_costs <- c(vv(AF_planning_costs, var_CV, 1), 
                      rep(0, (n_years - 1))) + # Y1
    c(vv(AF_area_on_AF_planning_costs, var_CV, 1), rep(0, (n_years - 1))) *
    log(production_area) 
  # production_area log() is fine minimum area is >= 5 so no issues with x <=1
  
  #### Tillage costs before planting
  tillage_costs <-  c(vv(tillage_costs_ha, var_CV, 1), 
                      rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  tillage_planting_preparation_costs <- c(vv(tillage_planting_preparation_costs_ha, 
                                             var_CV, 1),
                                          rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Weed control in 1st year
  weed_control_costs <- c(vv(weed_control_costs_ha, var_CV, 1), 
                          rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Seed costs
  seedling_costs_popplar_10_20Y <-  c(vv(seedlings_ha_poplar_10_20Y, var_CV, 1), 
                                      rep(0, (n_years - 1))) * # Y1
    agroforestry_area *
    c(vv(seedling_price_poplar_0.2m, var_CV, 1), rep(0, (n_years - 1)))
  
  #### Planting costs
  planting_costs_mechanical <-  c(vv(planting_costs_ha_mechanical, var_CV, 1), 
                                  rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Fencing costs
  fence_construction_costs <- c(vv(fence_construction_costs_ha, var_CV, 1), 
                                rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Tree upkeep costs in 1st year
  tree_upkeep_costs_1stY <- c(vv(tree_upkeep_costs_ha_1stY, var_CV, 1), 
                              rep(0, (n_years - 1))) * # Y1
    agroforestry_area
  
  #### Tree upkeep costs after 1st year
  tree_upkeep_costs_after_1stY <- c(0, vv(tree_upkeep_costs_ha_after_1stY, 
                                          var_CV, n_years - 1)) *
    agroforestry_area
  
  #### Harvest costs
  cutting_costs <-  vv(cutting_costs_taDM, var_CV, n_years) * 
    # Depending on the interval
    produced_poplar_MS
  
  chopping_costs <- vv(chopping_costs_taDM, var_CV, n_years) * 
    # Depending on the interval
    produced_poplar_MS
  
  storage_costs <- vv(storage_costs_taDM, var_CV, n_years) * 
    # Depending on the interval
    produced_poplar_MS
  
  #### Reconversion costs after the last year
  reconversion_cost <-  c(rep(0, n_years - 1), reconversion_cost_ha) * # Last year
    agroforestry_area
  
  ### Adding up costs tree ####
  
  costs_poplar <-  planning_costs +
    tillage_costs +
    tillage_planting_preparation_costs +
    weed_control_costs +
    seedling_costs_popplar_10_20Y +
    planting_costs_mechanical +
    fence_construction_costs +
    tree_upkeep_costs_1stY +
    tree_upkeep_costs_after_1stY +
    cutting_costs +
    chopping_costs +
    storage_costs +
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
  
  other_animal_products_benefits <- vv(other_animal_products_t_milk, 
                                       var_CV, n_years) *
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
  wood_chips_sales_benefits <- vv(wood_chips_price_t, var_CV, n_years) *
    (produced_poplar_MS / (1 - residual_moisture))
  
  ### Adding up benefits tree ####
  benefits_poplar <- agroforestry_subsidies +
    wood_chips_sales_benefits
  
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
  
  # AF Slows nutrient loss
  nutrient_loss_costs_AF <- vv(EcoB_nutrient_loss, var_CV, n_years) *
    (1 - vv(AF_on_nutrient_loss, var_CV, n_years))
  
  ## Biodiversity and aesthetics
  biodiversity_aesthetics_AF <- vv(EcoB_biodiversity_aesthetics, var_CV, n_years) *
    agroforestry_area
  
  ## Adding up ecological aspects
  
  ecological_aspects_no_AF <- 
    (erosion_costs_no_AF -
    nutrient_loss_costs_no_AF)
  
  ecological_aspects_AF <-
    (erosion_costs_AF +
    ground_water_gain_AF +
    pollination_AF +
    carbon_storage_AF  +
    biodiversity_aesthetics_AF) - nutrient_loss_costs_AF
  
  # Profits ####
  # livelihoods (income diverse)
  
  ## Profits no agroforestry
  profit_no_AF_final <- benefits_milk_no_AF - costs_milk_no_AF

  animal_welfare_no_AF   <- # Stress to the ainimal
    
  ## Profits agroforestry ####
  profit_AF_final_poplar <-  benefits_milk_AF - costs_milk_AF + benefits_poplar - costs_poplar
  
  # Net present value (NPV) ####
  NPV_AF_poplar <- discount(profit_AF_final_poplar - profit_no_AF_final,
                                   discount_rate,
                                   calculate_NPV = TRUE)
  
  # Ecology ####
  
  # Ecological effects ####
  ecological_AF_diff <- ecological_aspects_no_AF - ecological_aspects_AF
 
  ## Animal welfare effects ####
  # Stress to the animal THI as a proxy
  animal_welfare_no_AF <- milk_reduction_stress_kg_cow_no_AF #THI_table_stress_sum_no_AF * milk_reduction_THI_kg_cow_day
  animal_welfare_AF <- milk_reduction_stress_kg_cow_AF #THI_table_stress_sum_AF * milk_reduction_THI_kg_cow_day
  

  # Return ####
  return(
    list(
      NPV_AF_poplar = NPV_AF_poplar,
      Cashflow_AF = profit_AF_final_poplar,
      Annual_ecology_effect = ecological_AF_diff,
      ecological_AF_effect = sum(ecological_AF_diff)
    )
  )
  
  
  
}  # End function

# Monte Carlo simulation ####
dairy_mc_simulation <- mcSimulation(
  estimate = as.estimate(input_table),
  model_function = dairy_model,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

