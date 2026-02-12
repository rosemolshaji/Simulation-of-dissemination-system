#=====================================
# Visualization for the two-tier system
#=====================================
library(ggplot2)
library(forcats)
make_sd_barplot <- function(df, mean_col, sd_col, ylab, title, ylimits) {
  
  # Make a local copy of the data
  
  plot_df <- df
  
  # Relabel Fecundity as categorical: Low → Medium → High
  
  plot_df$Fecundity_Label <- factor(plot_df$Fecundity,
                                    levels = c(1500, 2250, 3000),
                                    labels = c("Low", "Medium", "High"))
  
  # Set desired internal order of survival scenarios (for bar order)
  
  plot_df$Survival_Set <- factor(plot_df$Survival_Set,
                                 levels = c("Low survival scenario",
                                            "Medium survival scenario",
                                            "High survival scenario"))
  
  # Recode levels for legend display
  
  plot_df$Survival_Set <- fct_recode(plot_df$Survival_Set,
                                     "Low survival (70%, 60%, 50%)" = "Low survival scenario",
                                     "Medium survival (80%, 70%, 60%)" = "Medium survival scenario",
                                     "High survival (90%, 80%, 70%)" = "High survival scenario"
  )
  
  # Custom fill color palette (matching recoded labels)
  
  fill_colors_named <- c("Low survival (70%, 60%, 50%)" = "#984ea3",   # purple
                         "Medium survival (80%, 70%, 60%)" = "#ff7f00", # orange
                         "High survival (90%, 80%, 70%)" = "#4daf4a")   # green
  
  # Plot
  ggplot(plot_df, aes(x = Fecundity_Label, y = .data[[mean_col]], fill = Survival_Set)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
    geom_errorbar(aes(ymin = .data[[mean_col]] - .data[[sd_col]],
                      ymax = .data[[mean_col]] + .data[[sd_col]]),
                  width = 0.2, position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = fill_colors_named) +
    labs(y = ylab, x = "Fecundity Level", title = title, fill = "Survival Scenario") +
    coord_cartesian(ylim = ylimits) +
    theme_bw()
}


# Area Plots (separately)
p_area_total <- make_sd_barplot(descriptive_stats_alltier1, "Mean_TotalArea", "SD_TotalArea", "Area (ha)", "Total area requirement (2-tier system)", c(1000, 3000))
print(p_area_total)

p_area_nucleus <- make_sd_barplot(descriptive_stats_alltier1, "Mean_AreaNucleus", "SD_AreaNucleus", "Area (ha)", "Area required in nucleus unit", c(0, 900))
print(p_area_nucleus)

p_area_growout <- make_sd_barplot(descriptive_stats_alltier1, "Mean_AreaGrowout", "SD_AreaGrowout", "Area (ha)", "Area required in grow-out unit", c(800, 2000))
print(p_area_growout)

# Brooder Plots (separately)
p_brood_nucleus <- make_sd_barplot(descriptive_stats_alltier1, "Mean_FemalesNuc", "SD_FemalesNuc", "Number of brooders (thousands)", "Brooder requirement in nucleus", c(0, 2700))
print(p_brood_nucleus)

# Fry Plots (separately)
p_fry_prod <- make_sd_barplot(descriptive_stats_alltier1, "Mean_FryProd", "SD_FryProd", "Number of fry (millions)", "Fry requirement in production", c(0, 600))
print(p_fry_prod)

#==================================
# Visualization for the three-tier system
#==================================

make_sd_barplot <- function(df, mean_col, sd_col, ylab, title, ylimits) {
  
  # Make a local copy of the data
  plot_df <- df
  
  # Relabel Fecundity as categorical: Low → Medium → High
  plot_df$Fecundity_Label <- factor(plot_df$Fecundity,
                                    levels = c(1500, 2250, 3000),
                                    labels = c("Low", "Medium", "High"))
  
  # Set desired internal order of survival scenarios (for bar order)
  plot_df$Survival_Set <- factor(plot_df$Survival_Set,
                                 levels = c("Low survival scenario",
                                            "Medium survival scenario",
                                            "High survival scenario"))
  
  # Recode levels for legend display
  plot_df$Survival_Set <- fct_recode(plot_df$Survival_Set,
                                     "Low survival (70%, 60%, 50%)" = "Low survival scenario",
                                     "Medium survival (80%, 70%, 60%)" = "Medium survival scenario",
                                     "High survival (90%, 80%, 70%)" = "High survival scenario"
  )
  
  # Custom fill color palette (matching recoded labels)
  fill_colors_named <- c("Low survival (70%, 60%, 50%)" = "forestgreen",   # purple
                         "Medium survival (80%, 70%, 60%)" = "#ff7f00", # orange
                         "High survival (90%, 80%, 70%)" = "tomato")   # green
  
  # Plot
  ggplot(plot_df, aes(x = Fecundity_Label, y = .data[[mean_col]], fill = Survival_Set)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
    geom_errorbar(aes(ymin = .data[[mean_col]] - .data[[sd_col]],
                      ymax = .data[[mean_col]] + .data[[sd_col]]),
                  width = 0.2, position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = fill_colors_named) +
    labs(y = ylab, x = "Fecundity Level", title = title, fill = "Survival Scenario") +
    coord_cartesian(ylim = ylimits) +
    theme_bw()
}


p1 <- make_sd_barplot(descriptive_stats_all, "Mean_FryMult", "SD_FryMult", "Fry (millions)", "Fry required in multiplier unit", c(0, 100))
p2 <- make_sd_barplot(descriptive_stats_all, "Mean_FemalesMultAdj", "SD_FemalesMultAdj", "Brooders (thousands)", "Brooders in multiplier unit", c(0, 10000))
p3 <- make_sd_barplot(descriptive_stats_all, "Mean_FryProd", "SD_FryProd", "Fry (millions)", "Fry needed for production", c(0, 500))
p4 <- make_sd_barplot(descriptive_stats_all, "Mean_FemalesNuc", "SD_FemalesNuc", "Brooders (thousands)", "Brooders in nucleus", c(0, 1250))
p5 <- make_sd_barplot(
  descriptive_stats_all,
  "Mean_AreaMultiplier", "SD_AreaMultiplier",
  "Area (ha)", "Area required in multiplier unit",
  ylimits = c(0, 900)
)
p6 <- make_sd_barplot(
  descriptive_stats_all,
  "Mean_AreaNucleus", "SD_AreaNucleus",
  "Area (ha)", "Area required in nucleus unit",
  ylimits = c(0, 250)
)
p7 <- make_sd_barplot(
  descriptive_stats_all,
  "Mean_AreaGrowout", "SD_AreaGrowout",
  "Area (ha)", "Area required in grow-out unit",
  ylimits = c(800, 2000)
)
p8 <- make_sd_barplot(
  descriptive_stats_all,
  "Mean_TotalArea", "SD_TotalArea",
  "Area (ha)", "Total area requirement (3-tier system)",
  ylimits = c(1000, 3000)
)
print(p1)
print(p2)
print(p3)
print(p4)
print(p8)
print(p7)
print(p6)
print(p5)

#==================================
# Visualization for the four-tier system
#==================================

make_sd_barplot <- function(df, mean_col, sd_col, ylab, title, ylimits) {
  
  # Make a local copy of the data
  plot_df <- df
  
  # Relabel Fecundity as categorical: Low → Medium → High
  plot_df$Fecundity_Label <- factor(plot_df$Fecundity,
                                    levels = c(1500, 2250, 3000),
                                    labels = c("Low", "Medium", "High"))
  
  # Set desired internal order of survival scenarios (for bar order)
  plot_df$Survival_Set <- factor(plot_df$Survival_Set,
                                 levels = c("Low survival scenario",
                                            "Medium survival scenario",
                                            "High survival scenario"))
  
  # Recode levels for legend display
  plot_df$Survival_Set <- fct_recode(plot_df$Survival_Set,
                                     "Low survival (70%, 60%, 50%)" = "Low survival scenario",
                                     "Medium survival (80%, 70%, 60%)" = "Medium survival scenario",
                                     "High survival (90%, 80%, 70%)" = "High survival scenario"
  )
  
  # Custom fill color palette (matching recoded labels)
  fill_colors_named <- c("Low survival (70%, 60%, 50%)" = "#1b9e77",   # purple
                         "Medium survival (80%, 70%, 60%)" = "#d95f02", # orange
                         "High survival (90%, 80%, 70%)" = "#7570b3")   # green
  
  # Plot
  ggplot(plot_df, aes(x = Fecundity_Label, y = .data[[mean_col]], fill = Survival_Set)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
    geom_errorbar(aes(ymin = .data[[mean_col]] - .data[[sd_col]],
                      ymax = .data[[mean_col]] + .data[[sd_col]]),
                  width = 0.2, position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = fill_colors_named) +
    labs(y = ylab, x = "Fecundity Level", title = title, fill = "Survival Scenario") +
    coord_cartesian(ylim = ylimits) +
    theme_bw()
}

# Area Plots (separately)
p_area_total <- make_sd_barplot(descriptive_stats_allmod3, "Mean_TotalArea", "SD_TotalArea", "Area (ha)", "Total area requirement (4-tier system)", c(1000, 3000))
print(p_area_total)

p_area_nucleus <- make_sd_barplot(descriptive_stats_allmod3, "Mean_AreaNucleus", "SD_AreaNucleus", "Area (ha)", "Area required in nucleus unit", c(0, 100))
print(p_area_nucleus)

p_area_multiplier <- make_sd_barplot(descriptive_stats_allmod3, "Mean_AreaMultiplier", "SD_AreaMultiplier", "Area (ha)", "Area required in multiplier unit", c(0, 750))
print(p_area_multiplier)

p_area_growout <- make_sd_barplot(descriptive_stats_allmod3, "Mean_AreaGrowout", "SD_AreaGrowout", "Area (ha)", "Area required in grow-out unit", c(800, 2000))
print(p_area_growout)

# Brooder Plots (separately)
p_brood_nucleus <- make_sd_barplot(descriptive_stats_allmod3, "Mean_FemalesNuc", "SD_FemalesNuc", "Number of brooders (thousands)", "Brooder requirement in nucleus", c(0, 580))
print(p_brood_nucleus)

p_brood_sbn <- make_sd_barplot(descriptive_stats_allmod3, "Mean_females_needed_sbn", "SD_females_needed_sbn", "Number of brooders (thousands)", "Brooder requirement in SBN", c(0, 600))
print(p_brood_sbn)

p_brood_multiplier <- make_sd_barplot(descriptive_stats_allmod3, "Mean_FemalesMultAdj", "SD_FemalesMultAdj", "Number of brooders (thousands)", "Brooder requirement in multiplier unit", c(0, 10000))
print(p_brood_multiplier)

# Fry Plots (separately)
p_fry_sbn <- make_sd_barplot(descriptive_stats_allmod3, "Mean_Frysbn", "SD_Frysbn", "Number of fry (millions)", "Fry requirement in SBN", c(0, 25))
print(p_fry_sbn)

p_fry_multiplier <- make_sd_barplot(descriptive_stats_allmod3, "Mean_FryMult", "SD_FryMult", "Number of fry (millions)", "Fry requirement in multiplier unit", c(0, 90))
print(p_fry_multiplier)

p_fry_prod <- make_sd_barplot(descriptive_stats_allmod3, "Mean_FryProd", "SD_FryProd", "Number of fry (millions)", "Fry requirement in production", c(0, 400))
print(p_fry_prod)
