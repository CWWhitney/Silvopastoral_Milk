# Inputs: Temperature and Relative Humidity
Tmax <- c(25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36) # in °C
RH <- c(25, 30, 35, 40, 45, 50, 55, 60) # in %

# Create a dataframe of all combinations
combinations <- expand.grid(Tmax = Tmax, RH = RH)

# Calculate THI using the formula: 
#(1.8 * Tmaxi + 32) - ((0.55 - 0.0055 * HR) * (1.8 * Tmaxi - 26.8)) #National Research Council (1971)
combinations$THI <- with(combinations, ((1.8 * Tmax + 32) - ((0.55 - 0.0055*HR) * (1.8*Tmax - 26))))

# Filter out rows where THI is less than 68
filtered_combinations <- subset(combinations, THI >= 68)

# Display the dataframe
print(combinations)

# Optionally, save the dataframe to a CSV file
write.csv(combinations, "data/THI_range_heat_stress1.csv", row.names = FALSE)


# Inputs
milk_yield_initial <- 100  # Milk yield in Kg/cow/day
num_cows <- 10             # Number of cows
reduction_min <- 0.08      # Minimum reduction per THI unit > 68 (Kg/cow/day/unit)
reduction_max <- 0.41      # Maximum reduction per THI unit > 68 (Kg/cow/day/unit)

# Generate a sequence of THI values >= 68
THI_values <- seq(68, max(filtered_combinations$THI), by = 1)

# Create a dataframe for THI and calculate yield reduction
milk_yield <- data.frame(
  THI = THI_values,
  Reduction_Min = (THI_values - 67) * reduction_min, # Reduction using min factor
  Reduction_Max = (THI_values - 67) * reduction_max  # Reduction using max factor
)

# Calculate milk yield per cow after reduction
milk_yield$Yield_Per_Cow_Min <- milk_yield_initial - milk_yield$Reduction_Min
milk_yield$Yield_Per_Cow_Max <- milk_yield_initial - milk_yield$Reduction_Max

# Calculate total milk yield for all cows
milk_yield$Total_Yield_Min <- milk_yield$Yield_Per_Cow_Min * num_cows
milk_yield$Total_Yield_Max <- milk_yield$Yield_Per_Cow_Max * num_cows

# Display the resulting dataframe
print(milk_yield)

# Optionally, save the result to a CSV file
write.csv(milk_yield, "Milk_Yield_Reduction_THI.csv", row.names = FALSE)
