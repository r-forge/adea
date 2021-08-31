# Read data
data('tokyo_libraries')
input <- tokyo_libraries[,1:4]
output <-tokyo_libraries[,5:6]

# Solve dea model using adea from adea package
sol.adea <- adea(input, output)
# Scores
sol.adea$eff
# Weights
cbind(sol.adea$ux, sol.adea$vy)
# Load ratios
sol.adea$load$ratios
# Load
sol.adea$load$load

