# Load necessary libraries
library(data.table)
library(tidyverse)

# Define years of interest
years <- c(2014:2018)

# Function to process data
process_agz_data <- function(file_path, source_label) {
  data.table::fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years) %>%
    mutate(Treatment = ifelse(HERD %in% c(1:10), "TRM", "CARM")) %>%
    group_by(YR, Treatment, HERD, ID) %>%
    summarize(total_dmi = sum(DMIkg.hd), .groups = "drop") %>%
    group_by(YR, Treatment, HERD) %>%
    summarize(herd_dmi = sum(total_dmi), .groups = "drop") %>%
    group_by(YR, Treatment) %>%
    summarize(trt_dmi = mean(herd_dmi), .groups = "drop") %>%
    mutate(Source = source_label, 
           Group = factor(paste(Treatment, source_label, sep = "-")))  # Change "_" to "-"
}

# Process both datasets
agz_noVar_dmi <- process_agz_data(
  "C:/01-APEX1605_CO_baseline/WtGain_Simulation/APEX1605_CO_all20/CONUNN_TGM.AGZ", "NoVar"
)

agz_spatVar_dmi <- process_agz_data(
  "C:/02-APEX1605_spatialtemp/APEX1605_CO_92 subareas_div/WtGain_Simulation/APEX1605_CO_all92/CONUNN_TGM.AGZ", "SpatVar"
)

# Combine datasets
dmi_combined <- bind_rows(agz_noVar_dmi, agz_spatVar_dmi)

# Define custom colors (Okabe-Ito color-blind friendly palette)
custom_colors <- c("TRM-NoVar" = "#E69F00",  # Light Orange
                   "TRM-SpatVar" = "#CC6600",  # Dark Orange
                   "CARM-NoVar" = "#009E73",  # Light Teal
                   "CARM-SpatVar" = "#007360")  # Dark Teal

# Ensure Group levels match colors
dmi_combined$Group <- factor(dmi_combined$Group, levels = names(custom_colors))

# Create ggplot visualization
ggplot(dmi_combined, aes(x = as.factor(YR), y = trt_dmi, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Year", y = "Annual Dry Matter Intake (kg/hd/year)", fill = "Grazing Treatment & Source") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, drop = FALSE) +  # Use color-blind friendly colors
  theme(text = element_text(size = 15, family = "serif"),
        legend.position = "bottom") +  # Move legend to the bottom
  scale_x_discrete(labels = years)  # Ensure correct x-axis labels



