# Load necessary libraries
library(data.table)
library(tidyverse)
library(patchwork)
library(ggsci)  # For nejm color palette
library(emmeans)

# Define years of interest
years <- 2014:2018

# ----- PROCESSING FOR DRY MATTER INTAKE -----
process_agz_data <- function(file_path, source_label) {
  fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years) %>%
    mutate(Treatment = ifelse(HERD %in% 1:10, "TRM", "CARM")) %>%
    group_by(YR, Treatment, HERD, ID) %>%
    summarize(total_dmi = sum(DMIkg.hd), .groups = "drop") %>%
    group_by(YR, Treatment, HERD) %>%
    summarize(herd_dmi = sum(total_dmi), .groups = "drop") %>%
    group_by(YR, Treatment) %>%
    summarize(trt_dmi = mean(herd_dmi), .groups = "drop") %>%
    mutate(Source = source_label, 
           Group = factor(paste(Treatment, source_label, sep = "-")))
}

# File paths for DMI
file_noVar_dmi <- "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all20_noCAGbm/CONUNN_TGM.AGZ"
file_spatVar_dmi <- "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all92_noCAGbm/CONUNN_TGM.AGZ"

# Process DMI
agz_noVar_dmi <- process_agz_data(file_noVar_dmi, "NoVar")
agz_spatVar_dmi <- process_agz_data(file_spatVar_dmi, "SpatVar")
dmi_combined <- rbind(agz_noVar_dmi, agz_spatVar_dmi)

# Custom colors for DMI plot
custom_colors <- c(
  "TRM-NoVar" = "#E69F00", "TRM-SpatVar" = "#CC6600", 
  "CARM-NoVar" = "#009E73", "CARM-SpatVar" = "#007360"
)
dmi_combined$Group <- factor(dmi_combined$Group, levels = names(custom_colors))

# DMI Plot
plot_dmi <- ggplot(dmi_combined, aes(x = as.factor(YR), y = trt_dmi, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Year", y = "Annual Dry Matter Intake (kg/hd/year)", fill = "Treatment-Simulation") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, drop = FALSE) +  
  theme(text = element_text(size = 20, family = "serif"), legend.position = "right") +  
  scale_x_discrete(labels = years)

# ----- PROCESSING FOR MEAN PERCENTAGE -----
process_data <- function(file_path, simulation_label) {
  data <- fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years) %>%
    mutate(
      Treatment = ifelse(HERD <= 10, "TRM", "CARM"),
      Intake = GZSLkg.ha + GZSDkg.ha,
      Simulation = simulation_label
    )
  
  herd_summary <- data %>%
    group_by(YR, HERD, Treatment, CPNM, Simulation) %>%
    summarise(HerdIntake = sum(Intake, na.rm = TRUE), .groups = "drop")
  
  herd_totals <- herd_summary %>%
    group_by(YR, HERD, Simulation) %>%
    summarise(TotalIntake = sum(HerdIntake), .groups = "drop")
  
  # FIXED: Now correctly joining herd_summary with herd_totals
  herd_percent <- herd_summary %>%
    left_join(herd_totals, by = c("YR", "HERD", "Simulation")) %>%
    mutate(Percentage = 100 * HerdIntake / TotalIntake)
  
  treatment_summary <- herd_percent %>%
    group_by(YR, Treatment, CPNM, Simulation) %>%
    summarise(
      MeanPercentage = mean(Percentage, na.rm = TRUE),
      SE = sd(Percentage, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  return(treatment_summary)
}

# File paths for percentage
file_base <- "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all20_noCAGbm/CONUNN_TGM.AGZ"
file_spatial <- "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all92_noCAGbm/CONUNN_TGM.AGZ"

# Process percentage data
data_base <- process_data(file_base, "No Variability")
data_spatial <- process_data(file_spatial, "Spatial Variability")
combined_data <- rbind(data_base, data_spatial)

# Percentage Plot
plot_percentage <- ggplot(combined_data, aes(x = as.factor(YR), y = MeanPercentage, fill = CPNM)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Treatment ~ Simulation) +
  labs(x = "Year", y = "Mean Percentage of Total Intake (% weight)", fill = "Plant Group") +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 20)) +
  scale_fill_nejm(name = "Plant Group")

# ----- COMBINE BOTH PLOTS -----
combined_plot <- (plot_dmi / plot_percentage) + 
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ") ")

# Print combined plot
combined_plot

## Linear regression analysis
mod_dietComp <- lm(MeanPercentage ~ Simulation * CPNM * Treatment + YR, combined_data)
summary(mod_dietComp)

# Get estimated marginal means from each model
emm_dietComp <- emmeans(mod_dietComp, ~ Simulation | CPNM * Treatment) 

# Post-hoc analysis of pair-wise contrasts
pairs(emm_dietComp, adjust = "tukey", reverse = TRUE)

# Dry matter intake
mod_dmi <- lm(trt_dmi ~ Source  * Treatment + as.factor(YR), dmi_combined)
summary(mod_dmi)
