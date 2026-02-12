# ==========================================
# Simulation function for two-tier system
# ==========================================

simulate_production1tier <- function(params) {
  replicate(n_replicates, {
    
    # Sample biological parameters from Beta distributions
    hatch_rate <- rbeta(1, hatch_beta$alpha, hatch_beta$beta)
    survival_spawn <- rbeta(1, survival_spawn_beta$alpha, survival_spawn_beta$beta)
    survival_fry <- rbeta(1, survival_fry_beta$alpha, survival_fry_beta$beta)
    reproductive_efficiency <- rbeta(1, repro_beta$alpha, repro_beta$beta)
    genetic_gain <- rbeta(1, gain_beta$alpha, gain_beta$beta)
    average_genetic_merit<-n_generations*genetic_gain*mean_market_weight
    
    # Adjusted market weights with genetic lag per tier
    adjusted_weight_nucleus = mean_market_weight * (genetic_gain * n_generations + 1)
    adjusted_weight_multiplier <- adjusted_weight_nucleus
    adjusted_weight_production <- adjusted_weight_nucleus
    
    # Production tier calculations (growout)
    
    required_fish_prod <- target_harvest / adjusted_weight_production
    fingerlings_needed_prod <- required_fish_prod / params$survival_growout
    fry_needed_prod <- fingerlings_needed_prod / survival_fry
    
    
    # Nucleus tier calculations
    
    spawn_nuc <- fry_needed_prod / survival_fry
    eggs_nuc <- spawn_nuc / hatch_rate
    females_nuc <- eggs_nuc / params$fecundity
    total_females_nuc <- females_nuc / reproductive_efficiency
    males_nuc <- total_females_nuc
    total_brooders_nuc <- males_nuc + total_females_nuc
    harvestable_nuc <- total_females_nuc / 0.5
    fingerlings_nuc <- harvestable_nuc / params$survival_nucleus
    fry_nuc1 <- fingerlings_nuc / survival_fry
    spawn_nuc1 <- fry_nuc1 / survival_spawn
    eggs_nuc1 <- spawn_nuc1 / hatch_rate
    females_nuc1 <- eggs_nuc1 / params$fecundity
    total_females_nuc1 <- females_nuc1 / reproductive_efficiency
    male_nuc1 <- total_females_nuc1
    total_brooder_nuc1 <- male_nuc1 + total_females_nuc1
    total_final_brooder <- total_brooder_nuc1 + total_brooders_nuc
    average_genetic_merit<-average_genetic_merit
    
    # Area estimates (using your original formulas)
    area_nucleus <- sum(
      total_final_brooder / 5,
      spawn_nuc1 / 2000,
      fry_nuc1 / 200,
      fingerlings_nuc / 10,
      spawn_nuc / 2000
    )
  area_growout <- sum(
      fry_needed_prod / 200,
      fingerlings_needed_prod / 10
    )
    tibble(
      Genetic_Gain = params$genetic_gain,
      Fecundity = params$fecundity,
      Survival_Set = params$survival_combo,
      average_genetic_merit=average_genetic_merit,
      
      Adj_Weight_Nucleus = adjusted_weight_nucleus,
      Adj_Weight_Production = adjusted_weight_production,
      
      Required_Market_Fish_Prod = required_fish_prod,
      fry_Prod = fry_needed_prod,
      Total_Females_Nucleus = total_females_nuc1,
      Area_Nucleus = area_nucleus,
      Area_Growout = area_growout,
      Total_Area = area_nucleus  + area_growout,
      hatch_rate= hatch_rate,
      survival_fry=survival_fry,
      survival_spawn= survival_spawn,
      Reproductive_Efficiency = reproductive_efficiency
    )
  }, simplify = FALSE) %>% bind_rows()
}

# ==========================================
# Run simulation for all scenarios
# ==========================================

simulation_resultstier2 <- scenarios %>%
  pmap_dfr(~ simulate_production1tier(list(...))) %>%
  group_by( Fecundity, Survival_Set) %>%
  mutate(Replicate = row_number()) %>%
  ungroup()
write.csv(simulation_resultstier2,"simulation_results_tier2.csv")
