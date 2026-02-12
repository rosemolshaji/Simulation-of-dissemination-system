#================================
# Simulation function for four-tier system
#================================

simulate_production <- function(params) {
  replicate(n_replicates, {
    # Sample biological parameters from Beta distributions
    hatch_rate <- rbeta(1, hatch_beta$alpha, hatch_beta$beta)
    survival_spawn <- rbeta(1, survival_spawn_beta$alpha, survival_spawn_beta$beta)
    survival_fry <- rbeta(1, survival_fry_beta$alpha, survival_fry_beta$beta)
    reproductive_efficiency <- rbeta(1, repro_beta$alpha, repro_beta$beta)
    
    genetic_gain <- rbeta(1, gain_beta$alpha, gain_beta$beta)
    # Adjusted market weights with genetic lag per tier
    adjusted_weight_nucleus = mean_market_weight * (genetic_gain * n_generations + 1)
    adjusted_weight_multiplier <- adjusted_weight_nucleus
    adjusted_weight_production <- adjusted_weight_nucleus
    
    # Production tier calculations (growout)
    required_fish_prod <- target_harvest / adjusted_weight_production
    fingerlings_needed_prod <- required_fish_prod / params$survival_growout
    fry_needed_prod <- fingerlings_needed_prod / survival_fry
    
    # Multiplier tier calculations
    spawn_mult <- fry_needed_prod / survival_fry
    eggs_mult <- spawn_mult / hatch_rate
    females_mult <- eggs_mult / params$fecundity
    females_mult_adj <- females_mult / reproductive_efficiency
    males_mult <- females_mult_adj
    total_brooders_mult <- males_mult + females_mult
    harvestable_mult <- females_mult_adj / 0.5
    fingerlings_mult <- harvestable_mult / params$survival_multiplier
    fry_mult <- fingerlings_mult / survival_fry
    
    # Satellite Breeding Nucleus stage
    spawn_needed_sbn <- fry_mult / (survival_fry)
    eggs_needed_sbn <- spawn_needed_sbn / (hatch_rate)
    females_needed_sbn <- eggs_needed_sbn / params$fecundity
    total_broodstock_sbn <- females_needed_sbn / (reproductive_efficiency )
    males_sbn <- total_broodstock_sbn
    total_brooders_sbn <- males_sbn + total_broodstock_sbn
    harvestable_needed_sbn <- total_broodstock_sbn / 0.5
    fingerlings_needed_sbn <- harvestable_needed_sbn / params$survival_nucleus
    fry_needed_sbn1 <- fingerlings_needed_sbn / (survival_fry )
    
    # Nucleus tier calculations
    spawn_nuc <- fry_needed_sbn1 / survival_fry
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
    males_nuc1 <- total_females_nuc1
    total_brooders_nuc1 <- males_nuc1 + total_females_nuc1
    final_brooders_nuc <- total_brooders_nuc1+ total_brooders_nuc
    # Area estimates (using your original formulas)
    area_nucleus <- sum(
      final_brooders_nuc / 5,
      spawn_nuc1 / 2000,
      fry_nuc1 / 200,
      fingerlings_nuc / 10,
      spawn_nuc / 2000
    )
    area_sbn <- fry_needed_sbn1 / 200 + fingerlings_needed_sbn / 10 + total_brooders_sbn / 5 + spawn_needed_sbn / 2000
    area_multiplier <- sum(
      fry_mult / 200,
      fingerlings_mult / 10,
      total_brooders_mult / 5,
      spawn_mult / 2000
    )
    area_growout <- sum(
      fry_needed_prod / 200,
      fingerlings_needed_prod / 10
    )
    
    tibble(
      Genetic_Gain = params$genetic_gain,
      Fecundity = params$fecundity,
      Survival_Set = params$survival_combo,
      
      Adj_Weight_Nucleus = adjusted_weight_nucleus,
      Adj_Weight_Multiplier = adjusted_weight_multiplier,
      Adj_Weight_Production = adjusted_weight_production,
      
      Required_Market_Fish_Prod = required_fish_prod,
      Fry_Mult = fry_mult,
      females_mult_adj=  females_mult_adj,
      fry_Prod = fry_needed_prod,
      fry_needed_sbn1= fry_needed_sbn1,
      females_needed_sbn= females_needed_sbn,
      
      Total_Females_Nucleus = total_females_nuc1,
      Area_Nucleus = area_nucleus,
      Area_Multiplier = area_multiplier,
      Area_Growout = area_growout,
      Area_sbn = area_sbn,
      Total_Area = area_nucleus + area_multiplier + area_growout+area_sbn,
      hatch_rate= hatch_rate,
      survival_fry=survival_fry,
      survival_spawn= survival_spawn,
      Reproductive_Efficiency = reproductive_efficiency
    )
  }, simplify = FALSE) %>% bind_rows()
}
#=================================
# Run simulation for all scenarios
#=================================

simulation_resultstier4 <- scenarios %>%
  pmap_dfr(~ simulate_production(list(...))) %>%
  group_by( Fecundity, Survival_Set) %>%
  mutate(Replicate = row_number()) %>%
  ungroup()
write.csv(simulation_resultstier4,"simulation_results_tier4.csv")
