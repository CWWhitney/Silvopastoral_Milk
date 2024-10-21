# Heat stress tables

# Heat stress + AF on heat stress ####

create_heat_stress_tables <- function(var_CV, n_years){

heat_table_1 <- tibble(
  stress_days_25GC_2025 = vv(heat_stress_days_25GC_2025, var_CV, n_years),
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

heat_table_1[heat_table_1 <= 0] <- 0


## Heat stress days no AF ###

heat_table_no_AF_1 <- tibble(
  heat_stress_days_25GC = c(
    heat_table_1$stress_days_25GC_2025[c(1:5)],
    heat_table_1$stress_days_25GC_2030[c(6:10)],
    heat_table_1$stress_days_25GC_2035[c(11:15)],
    heat_table_1$stress_days_25GC_2040[c(16:20)],
    heat_table_1$stress_days_25GC_2045[c(21:25)],
    heat_table_1$stress_days_25GC_2050[c(26:30)]
  ),
  heat_stress_days_26GC = c(
    heat_table_1$stress_days_26GC_2025[c(1:5)],
    heat_table_1$stress_days_26GC_2030[c(6:10)],
    heat_table_1$stress_days_26GC_2035[c(11:15)],
    heat_table_1$stress_days_26GC_2040[c(16:20)],
    heat_table_1$stress_days_26GC_2045[c(21:25)],
    heat_table_1$stress_days_26GC_2050[c(26:30)]
  ),
  heat_stress_days_27GC = c(
    heat_table_1$stress_days_27GC_2025[c(1:5)],
    heat_table_1$stress_days_27GC_2030[c(6:10)],
    heat_table_1$stress_days_27GC_2035[c(11:15)],
    heat_table_1$stress_days_27GC_2040[c(16:20)],
    heat_table_1$stress_days_27GC_2045[c(21:25)],
    heat_table_1$stress_days_27GC_2050[c(26:30)]
  ),
  heat_stress_days_28GC = c(
    heat_table_1$stress_days_28GC_2025[c(1:5)],
    heat_table_1$stress_days_28GC_2030[c(6:10)],
    heat_table_1$stress_days_28GC_2035[c(11:15)],
    heat_table_1$stress_days_28GC_2040[c(16:20)],
    heat_table_1$stress_days_28GC_2045[c(21:25)],
    heat_table_1$stress_days_28GC_2050[c(26:30)]
  ),
  heat_stress_days_29GC = c(
    heat_table_1$stress_days_29GC_2025[c(1:5)],
    heat_table_1$stress_days_29GC_2030[c(6:10)],
    heat_table_1$stress_days_29GC_2035[c(11:15)],
    heat_table_1$stress_days_29GC_2040[c(16:20)],
    heat_table_1$stress_days_29GC_2045[c(21:25)],
    heat_table_1$stress_days_29GC_2050[c(26:30)]
  ),
  heat_stress_days_30GC = c(
    heat_table_1$stress_days_30GC_2025[c(1:5)],
    heat_table_1$stress_days_30GC_2030[c(6:10)],
    heat_table_1$stress_days_30GC_2035[c(11:15)],
    heat_table_1$stress_days_30GC_2040[c(16:20)],
    heat_table_1$stress_days_30GC_2045[c(21:25)],
    heat_table_1$stress_days_30GC_2050[c(26:30)]
  ),
  heat_stress_days_31GC = c(
    heat_table_1$stress_days_31GC_2025[c(1:5)],
    heat_table_1$stress_days_31GC_2030[c(6:10)],
    heat_table_1$stress_days_31GC_2035[c(11:15)],
    heat_table_1$stress_days_31GC_2040[c(16:20)],
    heat_table_1$stress_days_31GC_2045[c(21:25)],
    heat_table_1$stress_days_31GC_2050[c(26:30)]
  ),
  heat_stress_days_32GC = c(
    heat_table_1$stress_days_32GC_2025[c(1:5)],
    heat_table_1$stress_days_32GC_2030[c(6:10)],
    heat_table_1$stress_days_32GC_2035[c(11:15)],
    heat_table_1$stress_days_32GC_2040[c(16:20)],
    heat_table_1$stress_days_32GC_2045[c(21:25)],
    heat_table_1$stress_days_32GC_2050[c(26:30)]
  ),
  heat_stress_days_33GC = c(
    heat_table_1$stress_days_33GC_2025[c(1:5)],
    heat_table_1$stress_days_33GC_2030[c(6:10)],
    heat_table_1$stress_days_33GC_2035[c(11:15)],
    heat_table_1$stress_days_33GC_2040[c(16:20)],
    heat_table_1$stress_days_33GC_2045[c(21:25)],
    heat_table_1$stress_days_33GC_2050[c(26:30)]
  ),
  heat_stress_days_34GC = c(
    heat_table_1$stress_days_34GC_2025[c(1:5)],
    heat_table_1$stress_days_34GC_2030[c(6:10)],
    heat_table_1$stress_days_34GC_2035[c(11:15)],
    heat_table_1$stress_days_34GC_2040[c(16:20)],
    heat_table_1$stress_days_34GC_2045[c(21:25)],
    heat_table_1$stress_days_34GC_2050[c(26:30)]
  ),
  heat_stress_days_35GC = c(
    heat_table_1$stress_days_35GC_2025[c(1:5)],
    heat_table_1$stress_days_35GC_2030[c(6:10)],
    heat_table_1$stress_days_35GC_2035[c(11:15)],
    heat_table_1$stress_days_35GC_2040[c(16:20)],
    heat_table_1$stress_days_35GC_2045[c(21:25)],
    heat_table_1$stress_days_35GC_2050[c(26:30)]
  ),
  heat_stress_days_36GC = c(
    heat_table_1$stress_days_36GC_2025[c(1:5)],
    heat_table_1$stress_days_36GC_2030[c(6:10)],
    heat_table_1$stress_days_36GC_2035[c(11:15)],
    heat_table_1$stress_days_36GC_2040[c(16:20)],
    heat_table_1$stress_days_36GC_2045[c(21:25)],
    heat_table_1$stress_days_36GC_2050[c(26:30)]
  )
)

return(list(heat_table_1 = heat_table_1, 
       heat_table_no_AF_1 = heat_table_no_AF_1))

}