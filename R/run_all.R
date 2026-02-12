# ===============================
# Run Complete Dissemination Model
# ===============================

cat("Starting dissemination simulation...\n")

# Load parameters
source("R/parameters_setup.R")

# Run simulations
source("R/simulation_tier2.R")
source("R/simulation_tier3.R")
source("R/simulation_tier4.R")

# Generate descriptive statistics
source("R/descriptive_statistics.R")

# Generate visualisations
source("R/visualisation.R")

cat("All simulations completed successfully.\n")
