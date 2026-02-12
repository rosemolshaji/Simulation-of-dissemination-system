#====================================
#Descriptive statistics for each model in the two-tier system
#====================================

library(writexl)
library(modeest)
library(dplyr)
descriptive_stats_alltier2 <- simulation_resultstier2 %>%
  group_by(Fecundity, Survival_Set) %>%
  summarise(
    # Fry Needed for Production
    Mean_FryProd = mean(fry_Prod/1e6),
    SD_FryProd = sd(fry_Prod/1e6),
    Mode_Fry_Prod = mlv(fry_Prod / 1e6, method = "kernel")[[1]],
    Min_FryProd = min(fry_Prod/1e6),
    Max_FryProd = max(fry_Prod/1e6),
        
    # Total Females in Nucleus
    Mean_FemalesNuc = mean(Total_Females_Nucleus/1e3),
    SD_FemalesNuc = sd(Total_Females_Nucleus/1e3),
    Mode_Total_Females_Nucleus = mlv(Total_Females_Nucleus / 1e3, method = "kernel")[[1]],
    Min_FemalesNuc = min(Total_Females_Nucleus/1e3),
    Max_FemalesNuc = max(Total_Females_Nucleus/1e3),
    
    # Area estimates
    Mean_AreaNucleus = mean(Area_Nucleus/1e4),
    SD_AreaNucleus = sd(Area_Nucleus/1e4),
    Mode_Area_Nucleus = mlv(Area_Nucleus / 1e4, method = "kernel")[[1]],
    Min_AreaNucleus = min(Area_Nucleus/1e4),
    Max_AreaNucleus = max(Area_Nucleus/1e4),
        
    Mean_AreaGrowout = mean(Area_Growout/1e4),
    SD_AreaGrowout = sd(Area_Growout/1e4),
    Mode_Area_Growout = mlv(Area_Growout / 1e4, method = "kernel")[[1]],
    Min_AreaGrowout = min(Area_Growout/1e4),
    Max_AreaGrowout = max(Area_Growout/1e4),
        
    Mean_TotalArea = mean(Total_Area/1e4),
    SD_TotalArea = sd(Total_Area/1e4),
    Mode_Total_Area = mlv(Total_Area / 1e4, method = "kernel")[[1]],
    Min_TotalArea = min(Total_Area/1e4),
    Max_TotalArea = max(Total_Area/1e4),    
      .groups = "drop"
    )
write.csv(descriptive_stats_alltier2, "descriptive_stats_all_outputstier2.csv", row.names = FALSE)

#====================================
#Descriptive statistics for each model in the three-tier system
#====================================
descriptive_stats_alltier3 <- simulation_resultstier3 %>%
  group_by(Fecundity, Survival_Set) %>%
  summarise(
    # Fry Mult
    Mean_FryMult = mean(Fry_Mult/1e6),
    SD_FryMult = sd(Fry_Mult/1e6),
    Mode_FryMult = mlv(Fry_Mult/1e6, method = "kernel")[[1]],
    Min_FryMult = min(Fry_Mult/1e6),
    Max_FryMult = max(Fry_Mult/1e6),
    
    # Females Multiplier Adjusted
    Mean_FemalesMultAdj = mean(females_mult_adj/1e3),
    SD_FemalesMultAdj = sd(females_mult_adj/1e3),
      Mode_FemalesMultAdj = mlv(females_mult_adj/1e3, method = "kernel")[[1]],
    Min_FemalesMultAdj = min(females_mult_adj/1e3),
    Max_FemalesMultAdj = max(females_mult_adj/1e3),

    # Fry Needed for Production
    Mean_FryProd = mean(fry_Prod/1e6),
    SD_FryProd = sd(fry_Prod/1e6),
    Mode_FryProd = mlv(fry_Prod/1e6, method = "kernel")[[1]],
    Min_FryProd = min(fry_Prod/1e6),
    Max_FryProd = max(fry_Prod/1e6),
    
    # Total Females in Nucleus
    Mean_FemalesNuc = mean(Total_Females_Nucleus/1e3),
    SD_FemalesNuc = sd(Total_Females_Nucleus/1e3),
    Mode_FemalesNuc = mlv(Total_Females_Nucleus/1e3, method = "kernel", bw= 748)[[1]],
    Min_FemalesNuc = min(Total_Females_Nucleus/1e3),
    Max_FemalesNuc = max(Total_Females_Nucleus/1e3),
    
    # Area estimates
    Mean_AreaNucleus = mean(Area_Nucleus/1e4),
    SD_AreaNucleus = sd(Area_Nucleus/1e4),
    Mode_AreaNucleus = mlv(Area_Nucleus/1e4, method = "kernel", bw= 55)[[1]],
    Min_AreaNucleus = min(Area_Nucleus/1e4),
    Max_AreaNucleus = max(Area_Nucleus/1e4),
    
    Mean_AreaMultiplier = mean(Area_Multiplier/1e4),
    SD_AreaMultiplier = sd(Area_Multiplier/1e4),
    Mode_AreaMultiplier = mlv(Area_Multiplier/1e4, method = "kernel")[[1]],
    Min_AreaMultiplier = min(Area_Multiplier/1e4),
    Max_AreaMultiplier = max(Area_Multiplier/1e4),
        
    Mean_AreaGrowout = mean(Area_Growout/1e4),
    SD_AreaGrowout = sd(Area_Growout/1e4),
    Mode_AreaGrowout = mlv(Area_Growout/1e4, method = "kernel")[[1]],
    Min_AreaGrowout = min(Area_Growout/1e4),
    Max_AreaGrowout = max(Area_Growout/1e4),
        
    Mean_TotalArea = mean(Total_Area/1e4),
    SD_TotalArea = sd(Total_Area/1e4),
    Mode_TotalArea = mlv(Total_Area/1e4, method = "kernel")[[1]],
    Min_TotalArea = min(Total_Area/1e4),
    Max_TotalArea = max(Total_Area/1e4),    
    .groups = "drop"
  )

print(descriptive_stats_all$Mode_AreaNucleus)
write.csv(descriptive_stats_alltier3, "descriptive_stats_all_outputstier3.csv", row.names = FALSE)

#====================================
# Descriptive statistics for each model in four-tier system
#====================================
descriptive_stats_alltier4 <- simulation_resultstier4 %>%
  group_by(Fecundity, Survival_Set) %>%
  summarise(
    # Fry Mult
    Mean_FryMult = mean(Fry_Mult/1e6),
    SD_FryMult = sd(Fry_Mult/1e6),
    Mode_FryMult = mlv(Fry_Mult/1e6, method = "kernel")[[1]],
    Min_FryMult = min(Fry_Mult/1e6),
    Max_FryMult = max(Fry_Mult/1e6),
    
    # Females Multiplier Adjusted
    Mean_FemalesMultAdj = mean(females_mult_adj/1e3),
    SD_FemalesMultAdj = sd(females_mult_adj/1e3),
    Mode_FemalesMultAdj = mlv(females_mult_adj/1e3, method = "kernel")[[1]],
    Min_FemalesMultAdj = min(females_mult_adj/1e3),
    Max_FemalesMultAdj = max(females_mult_adj/1e3),
        
    # Fry sbn
    Mean_Frysbn = mean(fry_needed_sbn1/1e6),
    SD_Frysbn = sd(fry_needed_sbn1/1e6),
    Mode_Frysbn = mlv(fry_needed_sbn1/1e6, method = "kernel")[[1]],
    Min_FryMsbn = min(fry_needed_sbn1/1e6),
    Max_Frysbn = max(fry_needed_sbn1/1e6),
        
    # Females sbn Adjusted
    Mean_females_needed_sbn = mean(females_needed_sbn/1e3),
    SD_females_needed_sbn = sd(females_needed_sbn/1e3),
    Mode_females_needed_sbn = mlv(females_needed_sbn/1e3, method = "kernel")[[1]],
    Min_females_needed_sbn = min(females_needed_sbn/1e3),
    Max_females_needed_sbn = max(females_needed_sbn/1e3),
        
    # Fry Needed for Production
    Mean_FryProd = mean(fry_Prod/1e6),
    SD_FryProd = sd(fry_Prod/1e6),
    Mode_FryProd = mlv(fry_Prod/1e6, method = "kernel")[[1]],
    Min_FryProd = min(fry_Prod/1e6),
    Max_FryProd = max(fry_Prod/1e6),
        
    # Total Females in Nucleus
    Mean_FemalesNuc = mean(Total_Females_Nucleus/1e3),
    SD_FemalesNuc = sd(Total_Females_Nucleus/1e3),
    Mode_FemalesNuc = mlv(Total_Females_Nucleus/1e3, method = "kernel", bw= 500)[[1]],
    Min_FemalesNuc = min(Total_Females_Nucleus/1e3),
    Max_FemalesNuc = max(Total_Females_Nucleus/1e3),
    
    # Area estimates
    Mean_AreaNucleus = mean(Area_Nucleus/1e4),
    SD_AreaNucleus = sd(Area_Nucleus/1e4),
    Mode_AreaNucleus = mlv(Area_Nucleus/1e4, method = "kernel")[[1]],
    Min_AreaNucleus = min(Area_Nucleus/1e4),
    Max_AreaNucleus = max(Area_Nucleus/1e4),    
    Mean_Area_sbn = mean(Area_sbn/1e4),
    SD_Area_sbn = sd(Area_sbn/1e4),
    Mode_Area_sbn = mlv(Area_sbn/1e4, method = "kernel")[[1]],
    Min_Area_sbn = min(Area_Multiplier/1e4),
    Max_Area_sbn = max(Area_Multiplier/1e4),
    
    Mean_AreaMultiplier = mean(Area_Multiplier/1e4),
    SD_AreaMultiplier = sd(Area_Multiplier/1e4),
    Mode_AreaMultiplier = mlv(Area_Multiplier/1e4, method = "kernel")[[1]],
    Min_AreaMultiplier = min(Area_Multiplier/1e4),
    Max_AreaMultiplier = max(Area_Multiplier/1e4),
    
    Mean_AreaGrowout = mean(Area_Growout/1e4),
    SD_AreaGrowout = sd(Area_Growout/1e4),
    Mode_AreaGrowout = mlv(Area_Growout/1e4, method = "kernel")[[1]],
    Min_AreaGrowout = min(Area_Growout/1e4),
    Max_AreaGrowout = max(Area_Growout/1e4),
    
    Mean_TotalArea = mean(Total_Area/1e4),
    SD_TotalArea = sd(Total_Area/1e4),
    Mode_TotalArea = mlv(Total_Area/1e4, method = "kernel")[[1]],
    Min_TotalArea = min(Total_Area/1e4),
    Max_TotalArea = max(Total_Area/1e4),
    
    .groups = "drop"
  )
write.csv(descriptive_stats_alltier4, "descriptive_stats_all_outputstier4.csv", row.names = FALSE)
