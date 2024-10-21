## Thermal Heat Index (THI) Tables ####

create_THI_tables <- function(var_CV, n_years){
  
THI_table_1 <- tibble(
  THI_stress_25GC = vv(THI_heat_stress_25GC, var_CV, n_years),
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

return(list(THI_table_stress_sum_no_AF = THI_table_stress_sum_no_AF, 
            THI_table_stress_sum_AF = THI_table_stress_sum_AF))

}