## Heat stress days AF Tables ####

create_af_heat_stress_tables <- function(var_CV, n_years){
  
AF_on_stress_days_raw_1 <- tibble(
  AF_on_stress_days_25GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_25GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_25GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_25GC_15_20,
          AF_on_days_25GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_26GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_26GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_26GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_26GC_15_20,
          AF_on_days_26GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_27GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_27GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_27GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_27GC_15_20,
          AF_on_days_27GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_28GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_28GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_28GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_28GC_15_20,
          AF_on_days_28GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_29GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_29GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_29GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_29GC_15_20,
          AF_on_days_26GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_30GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_30GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_30GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_30GC_15_20,
          AF_on_days_30GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_31GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_31GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_31GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_31GC_15_20,
          AF_on_days_31GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_32GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_32GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_32GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_32GC_15_20,
          AF_on_days_32GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_33GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_33GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_33GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_33GC_15_20,
          AF_on_days_33GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_34GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_34GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_34GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_34GC_15_20,
          AF_on_days_30GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_35GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_35GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_35GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_35GC_15_20,
          AF_on_days_35GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
  AF_on_stress_days_36GC = vv(
    ifelse(
      AF_area_perc < 0.1,
      AF_on_days_36GC_5_10,
      ifelse(
        AF_area_perc < 0.15,
        AF_on_days_36GC_10_15,
        ifelse(
          AF_area_perc < 0.20,
          AF_on_days_36GC_15_20,
          AF_on_days_36GC_20_25
        )
      )
    ),
    var_CV,
    n_years
  ),
)


AF_on_stress_days_raw_2 <- AF_on_stress_days <- unname(as.matrix(AF_on_stress_days_raw_1))

AF_on_stress_days_raw <- cbind(
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
  c(rep(0, n_years)),
  c(rep(0, n_years))
)

# reducing AF effect on heat stress based on poplar growth stage
for (y in 1:n_years) {
  for (x in 1:12) {
    if (y == 1) {
      AF_on_stress_days_raw[y, x] <- 0
    }
    if (y == 2) {
      AF_on_stress_days_raw[y, x] <- AF_on_stress_days_raw_2[y, x] * (1 / 3)
    }
    if (y == 3) {
      AF_on_stress_days_raw[y, x] <- AF_on_stress_days_raw_2[y, x] * (2 / 3)
    }
    if (y > 3) {
      AF_on_stress_days_raw[y, x] <- AF_on_stress_days_raw_2[y, x]
    }
  }
}


# Round the stress effect 

round_stress_effect_AF <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == "numeric"
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

AF_on_stress_days <- round_stress_effect_AF(AF_on_stress_days_raw, 0)


heat_table_no_AF <- unname(as.matrix(heat_table_no_AF_1))


heat_table_AF <- cbind(
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
  c(rep(0, n_years)),
  c(rep(0, n_years))
)



for (y in 1:n_years) {
  for (x in 1:12) {
    if (AF_on_stress_days[y, x] < 0) {
      AF_on_stress_days[y, x] <- 0
    }
    if (x + AF_on_stress_days[y, x] <= 12) {
      heat_table_AF[y, x] <- heat_table_no_AF[y, x + AF_on_stress_days[y, x]]
    }
  }
}

return(list(heat_table_AF = heat_table_AF, 
            heat_table_no_AF = heat_table_no_AF))

}
