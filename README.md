# Silvopastoral_Milk
This R code has been developed as a decision support tool for grassland-based dairy farmers 
in Eifel region of Germany who are considering the adoption of short rotation coppice (SRC) poplar agroforestry systems. 
The tool evaluates whether integrating poplar trees into pastureland can provide climate resilience, specifically 
in mitigating the adverse impacts of heat and drought on both livestock productivity and farm operating costs.

Agroforestry is modeled here not only as a climate adaptation strategy, 
but also as a long-term investment in sustainable farm management. In addition to protecting livestock 
from impacts of climate extremes, agroforestry offers other co-benefits not limited to:

Enhanced soil stability and erosion control

Increased biodiversity and habitat provisioning

Generation of carbon credits

Improved microclimatic conditions for animals and forage

Key Modeling Assumptions
Climate Events (Drought and Heatwaves):
The occurrence of drought and heat events is simulated probabilistically using the chance_event() function 
from the decisionSupport R package. These probabilities represent general risk conditions and 
are not linked to any specific global or regional climate model. Users should interpret the outputs as 
scenario-based explorations rather than precise predictions.

Conditional Probabilities of Impact:
The combined effects of drought and heat events are modeled using a Conditional Probability Table (CPT) 
generated with the make_CPT() function. This approach estimates the likelihood of farm-level impacts 
depending on the co-occurrence or absence of extreme weather events.

Cow Breed Neutrality:
The model does not assume any specific dairy breed. While different breeds have varying tolerances to heat and 
drought stress (e.g., in terms of milk production loss, water intake, or mortality rates), 
this tool is designed to be breed-agnostic to enhance general applicability.
Users are encouraged to adjust relevant parameters such as milk yield reductions, 
heat tolerance thresholds, or mortality rates to reflect the breed(s) specific to their operation in the input_table.csv.

This code is suitable for:

Farmers - to evaluate trade-offs of adopting agroforestry for their grassland.

Researchers or extension agents - to modeling climate resilience strategies in dairy systems.

Policymakers or advisors - seek evidence-based justifications for agroforestry incentives.
