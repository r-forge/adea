#Read Data
data('spanishuniversities2018')
input <- spanishuniversities2018[, c('TeachingStaff')]
output <- spanishuniversities2018[, c("FPU2018s", "FPI2018s", "Patents", "PhDTheses", "JCRs", "Sixs", "Projects")]
# Solve dea model using Benchmarking package
cat('\nSolution using dea provided by Benchmarking package\n')
cat('--- Scores ---\n')
(sol.dea <- dea(input, output,  RTS = 'crs', DUAL=TRUE))
cat('--- Weights ---\n')
cbind(sol.dea$ux, sol.dea$vy)

# Solve dea model
cat('\nSolution using adea\n')
cat('--- Scores ---\n')
(sol.adea <- adea(input, output))
cat('--- Weights ---\n')
cbind(sol.adea$ux, sol.adea$vy)
cat('--- Load ratios ---\n')
sol.adea$load$ratios
cat('--- Load level ---\n')
sol.adea$load$load

