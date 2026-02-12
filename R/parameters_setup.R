library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)
# ============================
# Parameters
# ============================
target_harvest <- 16000 * 1e6  # grams total target harvest
mean_market_weight <- 150      # grams per fish base weight
n_generations <- 5              # total generations in nucleus tier
n_replicates <- 10000             # number of simulation replicates

# ============================
# Biological variables (Defining means and SDs for parameterisation of Beta distributions)
# ============================

mean_hatch_rate <- 0.30       # proportion (30%)
sd_hatch_rate <- 0.05
mean_survival_spawn <- 0.30   # proportion (30%)
sd_survival_spawn <- 0.05
mean_survival_fry <- 0.50     # proportion (50%)
sd_survival_fry <- 0.08
reproductive_efficiency_mean <- 0.25
reproductive_efficiency_sd <- 0.05
genetic_gain_mean <- 0.08
sd_gain <- 0.01

# ============================
# Function to convert mean and sd to Beta alpha and beta
# ============================

mean_to_beta <- function(mean, sd) {
  var <- sd^2
  alpha <- ((1 - mean) / var - 1 / mean) * mean^2
  beta <- alpha * (1 / mean - 1)
  list(alpha = alpha, beta = beta)
}

# ============================
# Beta distribution parameters
# ============================

hatch_beta <- mean_to_beta(mean_hatch_rate, sd_hatch_rate)
survival_spawn_beta <- mean_to_beta(mean_survival_spawn, sd_survival_spawn)
survival_fry_beta <- mean_to_beta(mean_survival_fry, sd_survival_fry)
repro_beta <- mean_to_beta(reproductive_efficiency_mean, reproductive_efficiency_sd)
gain_beta <- mean_to_beta(genetic_gain_mean, sd_gain)

# ============================
# Survival combinations for scenarios (nucleus, multiplier, production)
# ============================

survival_combinations <- list(
  c(0.7, 0.6, 0.5),
  c(0.8, 0.7, 0.6),
  c(0.9, 0.8, 0.7)
)
# ============================
# Create all scenarios (survival sets x genetic gain x fecundity)
# ============================

scenarios <- expand.grid(
  survival_id = 1:3,
  #genetic_gain = c(0.05, 0.08, 0.11),
  fecundity = c(1500, 2250, 3000)
) %>%
  mutate(survival_nucleus = map_dbl(survival_id,~survival_combinations[[.x]][1]),
    survival_multiplier = map_dbl(survival_id, ~survival_combinations[[.x]][2]),
    survival_growout = map_dbl(survival_id, ~survival_combinations[[.x]][3]),
    survival_combo = case_when(
      survival_id == 1 ~ "Low survival scenario",
      survival_id == 2 ~ "Medium survival scenario",
      survival_id == 3 ~ "High survival scenario"
    )
  )
