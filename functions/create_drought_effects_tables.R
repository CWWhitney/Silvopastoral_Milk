# Drought effect tables ####

create_drought_effects_tables <- function(var_CV, n_years){
  
  # Auxiliary table ####
auxiliary_table <- tibble(
  year = c(1:n_years),
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
     vv(AF_on_drought_effect_milk_production, var_CV, n_years))
### Drought effects on  production AF
auxiliary_table$drought_effect_poplar_AF <- vv(drought_effect_poplar_production, var_CV, n_years)

auxiliary_table$drought_effect_poplar_died_perc_1stY <- c(drought_effect_poplar_died_perc_1stY, rep(0, (n_years - 1))) # Y1

auxiliary_table$drought_effect_poplar_died_perc_after_1stY <-  c(0,
                                                                 vv(
                                                                   drought_effect_poplar_died_perc_after_1stY,
                                                                   var_CV,
                                                                   n_years - 1
                                                                 )) # Ya1

auxiliary_table$drought_effect_poplar_died_yield_effect_perc <- vv(drought_effect_poplar_died_yield_effect_perc,
                                                                   var_CV,
                                                                   n_years)


## Seed coats
auxiliary_table$seed_costs_ha_pot <- vv(seed_costs_ha, var_CV, n_years)

auxiliary_table$drought_effect_seed_costs_nY <-  vv(drought_effect_seed_costs_next_year, var_CV, n_years)

auxiliary_table$AF_on_seed_costs_after_drought_nY <-  1 -
  vv(AF_on_seed_costs_after_drought, var_CV, n_years)




# For years; for loop for drought ####
for (y in 1:n_years) {
  ## Drought not depending in different Years ####
  if (auxiliary_table$drought_happened[y] == 1) {
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
  if (y == 1) {
    auxiliary_table$seed_costs_year_no_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
    
    auxiliary_table$seed_costs_year_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
    
    
    ### Drought happened 1st Y
    if (auxiliary_table$drought_happened[y] == 1) {
      #### Drought effects no AF # done in Drought not depending in different Years
      
      #### Drought effect AF
      ##### Reduced yield
      auxiliary_table$milk_t_ha_year_AF[y] <- milk_t_ha_grazing_pot[y] *
        auxiliary_table$drought_effect_milk_no_AF[y] # No AF because trees are to slam in Y1
      
      #### Poplars dying
      auxiliary_table$drought_effect_poplar_died_happende[y] <- chance_event(drought_effect_poplar_died_chance_1stY, 1, 0)
      
      if (auxiliary_table$drought_effect_poplar_died_happende[y] == 1) {
        auxiliary_table$drought_effect_poplar_died_perc[y] <-  auxiliary_table$drought_effect_poplar_died_perc_1stY[y]
      }
      else {
        auxiliary_table$drought_effect_poplar_died_perc[y] <- 0
      }
      
      #### Poplars dying effect on future yield
      if (auxiliary_table$drought_effect_poplar_died_happende[y] == 0) {
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = 1
      }
      else {
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] =  1 -
          (
            auxiliary_table$drought_effect_poplar_died_perc[y] *
              auxiliary_table$drought_effect_poplar_died_yield_effect_perc [y]
          )
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
    if (auxiliary_table$drought_happened[y - 1] == 1) {
      ### Seed costs for the year after the drought
      auxiliary_table$seed_costs_year_no_AF[y] <- auxiliary_table$seed_costs_ha_pot[y] +
        auxiliary_table$seed_costs_ha_pot[y] *
        auxiliary_table$drought_effect_seed_costs_nY[y]
      
      auxiliary_table$seed_costs_year_AF[y] <-  auxiliary_table$seed_costs_ha_pot[y] +
        auxiliary_table$seed_costs_ha_pot[y] *
        (
          auxiliary_table$drought_effect_seed_costs_nY[y] *
            auxiliary_table$AF_on_seed_costs_after_drought_nY[y]
        )
    }
    
    else {
      ## Seed costs without drought after Y1
      auxiliary_table$seed_costs_year_no_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
      
      auxiliary_table$seed_costs_year_AF[y] <- auxiliary_table$seed_costs_ha_pot[y]
    }
    
    
    # Drought happened in Y after 1st
    if (auxiliary_table$drought_happened[y] == 1) {
      # Drought effects no AF
      
      #### Drought effect AF
      ##### Reduced yield
      auxiliary_table$milk_t_ha_year_AF[y] <- milk_t_ha_grazing_pot[y] *
        auxiliary_table$drought_effect_milk_AF[y]
      
      
      ## Poplars dying
      auxiliary_table$drought_effect_poplar_died_happende[y] <- chance_event(drought_effect_poplar_died_chance_after_1stY, 1, 0)
      
      if (auxiliary_table$drought_effect_poplar_died_happende[y] == 1) {
        auxiliary_table$drought_effect_poplar_died_perc[y] <-  auxiliary_table$drought_effect_poplar_died_perc_after_1stY[y]
      }
      else {
        auxiliary_table$drought_effect_poplar_died_perc[y] <- 0
      }
      
      ## Poplars dying effect on future yield
      if (auxiliary_table$drought_effect_poplar_died_happende[y] == 0) {
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y -
                                                                                                                                                           1]
      }
      else {
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] =  auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y -
                                                                                                                                                            1] -
          (
            auxiliary_table$drought_effect_poplar_died_perc[y] *
              auxiliary_table$drought_effect_poplar_died_yield_effect_perc [y]
          )
      }
      ## Actual yield / biomass growth after 1stY with drought
      auxiliary_table$yield_poplar_MS_taDM_ha_year[y] <-  yield_poplar_MS_taDM_ha_pot[y] *
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] *
        auxiliary_table$drought_effect_poplar_AF[y]
    }
    
    # No drought happened in Y after 1st
    else {
      if (auxiliary_table$drought_effect_poplar_died_happende[y] == 0) {
        auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y] = auxiliary_table$drought_effect_poplar_died_permanent_yield_multiplier[y -
                                                                                                                                                           1]
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
  if (y == 5) {
    auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (
      auxiliary_table$yield_poplar_MS_taDM_ha_year[1] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[2] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[3] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[4] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[5]
    ) /
      2
  }
  if (y == 10) {
    auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (
      auxiliary_table$yield_poplar_MS_taDM_ha_year[1] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[2] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[3] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[4] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[5]
    ) /
      2 +
      (
        auxiliary_table$yield_poplar_MS_taDM_ha_year[6] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[7] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[8] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[9] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[10]
      ) /
      2
  }
  if (y == 15) {
    auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (
      auxiliary_table$yield_poplar_MS_taDM_ha_year[6] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[7] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[8] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[9] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[10]
    ) /
      2 +
      (
        auxiliary_table$yield_poplar_MS_taDM_ha_year[11] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[12] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[13] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[14] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[15]
      ) /
      2
  }
  if (y == 20) {
    auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (
      auxiliary_table$yield_poplar_MS_taDM_ha_year[11] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[12] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[13] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[14] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[15]
    ) /
      2 +
      (
        auxiliary_table$yield_poplar_MS_taDM_ha_year[16] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[17] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[18] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[19] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[20]
      ) /
      2
  }
  if (y == 25) {
    auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (
      auxiliary_table$yield_poplar_MS_taDM_ha_year[16] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[17] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[18] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[19] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[20]
    ) /
      2 +
      (
        auxiliary_table$yield_poplar_MS_taDM_ha_year[21] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[22] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[23] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[24] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[25]
      ) /
      2
  }
  if (y == 30) {
    auxiliary_table$yield_poplar_MS_taDM_ha_harvest_year[y] <- (
      auxiliary_table$yield_poplar_MS_taDM_ha_year[21] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[22] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[23] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[24] +
        auxiliary_table$yield_poplar_MS_taDM_ha_year[25]
    ) /
      2 +
      (
        auxiliary_table$yield_poplar_MS_taDM_ha_year[26] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[27] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[28] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[29] +
          auxiliary_table$yield_poplar_MS_taDM_ha_year[30]
      )
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


return(list(milk_t_ha_no_AF = milk_t_ha_no_AF,
            milk_t_ha_AF = milk_t_ha_AF,
            yield_poplar_MS_taDM_ha = yield_poplar_MS_taDM_ha, 
            seed_costs_no_AF = seed_costs_no_AF,
            seed_costs_AF = seed_costs_AF))

}