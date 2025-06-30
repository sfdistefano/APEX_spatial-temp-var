library(tidyverse)
library(reshape2)
library(data.table)
library(ggsci)
library(hydroGOF)
library(patchwork)
library(cowplot)

##### ANNUAL GRAZING FILE (.AGZ) ###############################################

# Importing reference information for pastures
PastureID_92sa <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv")
PastureID_20sa <- read.csv("C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv")

## Importing observed annual mean weight gain data
observed_weight_data <- read.csv("C:/APEX data and scripts/Data/CPER Cattle/CARM_Cattle Weight Gains_2014-2023_SD.csv") %>%
  mutate(gain = ADG / 2.2046) %>%  # Converting from lbs to kg
  merge(PastureID_20sa, by = 'Pasture', 
        relationship = "many-to-many", all.x = TRUE) %>%
  mutate(Treatment = ifelse(ID %in% c(1:10), "TRM", "CARM"))

# Summarizing observed data for each treatment type
observed_weight_data_filtered <- observed_weight_data %>%
  select(Year, Treatment, gain) %>%
  filter(Year %in% c(2014:2018)) %>%
  group_by(Year, Treatment) %>%
  summarize(MeanWT = mean(gain, na.rm = TRUE), 
            SD = sd(gain), 
            Type = "Measured", .groups = 'drop')

### Function to read and process simulated data by grazing treatment
prepare_weight_data_agz <- function(file_path, years, id_filter, 
                                    sim_type, Pasture_data) {
  data.table::fread(file_path, fill = TRUE, skip = 8, header = TRUE, check.names = TRUE) %>%
    filter(YR %in% years, ID %in% id_filter) %>%
    left_join(Pasture_data, by = "ID") %>%
    group_by(YR, Treatment) %>%
    summarize(MeanWT = mean(WTGkg.hd.d, na.rm = TRUE)) %>%
    mutate(Type = sim_type, SD = 0) %>%
    rename(Year = YR)
}
## Importing simulated data using the reusable function
years <- c(2014:2018)

# Baseline simulation scenario
grazing_weight_data_noVar_yearly <- prepare_weight_data_agz(
  "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all20_noCAGbm/CONUNN_TGM.AGZ",
  years, 1:20, Pasture_data = PastureID_20sa,
  "Simulated: no variability"
)

# Spatial variability simulation scenario
grazing_weight_data_spatVar_yearly <- prepare_weight_data_agz(
  "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all92_noCAGbm/CONUNN_TGM.AGZ",
  years, 1:92, Pasture_data = PastureID_92sa,
  "Simulated: spatial variability"
)

## Combining all grazing observed and simulated data
grazing_weight_data_yearly <- rbind(
  observed_weight_data_filtered, 
  grazing_weight_data_noVar_yearly, 
  grazing_weight_data_spatVar_yearly
)

# Reorder the levels of the Type column
grazing_weight_data_yearly$Type <- factor(grazing_weight_data_yearly$Type, 
                                          levels = c("Measured", 
                                                     "Simulated: no variability", 
                                                     "Simulated: spatial variability"
                                          ))

# Create a bar plot with error bars
# Create combined ggplot for TRM and CARM
ggplot_grazing_yearly <- ggplot(grazing_weight_data_yearly, aes(x = factor(Year), y = MeanWT, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bars for mean weight
  geom_errorbar(aes(ymin = MeanWT - SD, ymax = MeanWT + SD), 
                position = position_dodge(width = 0.9), width = 0.25) +  # Error bars
  facet_wrap(~ Treatment) +  # Facet by Treatment (TRM and CARM)
  labs(x = "Year", 
       y = "Mean Daily Weight Gain (kg/head/day)", 
       fill = "Data Source") +  # Labels and legend title
  theme_bw() +   # Clean theme
  scale_fill_npg() +
  theme(text = element_text(size = 15, family = 'serif'))

# Print the combined plot
print(ggplot_grazing_yearly)

ggsave(
  filename = "C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/1605 weight gain/Figure_avg wt gain.png",
  plot = ggplot_grazing_yearly,
  width = 15,       # width in inches 
  height = 7,      # height in inches 
  dpi = 300,       # publication resolution
  units = "in",
  bg = "white"     # ensure transparent background is not used
)

###### DAILY GRAZING FILE (.DGZ) ###############################################


# Define color blind friendly palette
cbPalette <- c("#E69F00", "#0072B2")

# Make sure Simulation factor levels match the palette order 
grazing_weight_data_daily$Simulation <- factor(grazing_weight_data_daily$Simulation,
                                               levels = unique(grazing_weight_data_daily$Simulation))

# --- Build the 4 panels ---

# Daily Weight Gain - CARM (no legend)
p1 <- ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'CARM', ], aes(x = Date)) +
  geom_smooth(aes(y = DWG, color = Simulation), method = "gam", se = FALSE, show.legend = FALSE) +
  scale_color_manual(values = cbPalette) +
  theme_minimal() +
  ylab("Daily Weight Gain (kg/hd/day)") +
  xlab("") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(
    text = element_text(size = 15, family = 'serif'),
    plot.title = element_blank()
  )

# Daily Weight Gain - TRM (no legend)
p2 <- ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'TRM', ], aes(x = Date)) +
  geom_smooth(aes(y = DWG, color = Simulation), method = "gam", se = FALSE, show.legend = FALSE) +
  scale_color_manual(values = cbPalette) +
  theme_minimal() +
  ylab("") +
  xlab("") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(
    text = element_text(size = 15, family = 'serif'),
    plot.title = element_blank()
  )

# Daily Animal Weight - CARM (keep legend)
p3 <- ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'CARM', ], aes(x = Date)) +
  geom_line(aes(y = GZWT, color = Simulation), linewidth = 0.75) +
  scale_color_manual(values = cbPalette) +
  theme_minimal() +
  ylab("Daily Animal Weight (kg/hd)") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(
    text = element_text(size = 15, family = 'serif'),
    plot.title = element_blank()
  )

# Daily Animal Weight - TRM (keep legend)
p4 <- ggplot(grazing_weight_data_daily[grazing_weight_data_daily$Treatment == 'TRM', ], aes(x = Date)) +
  geom_line(aes(y = GZWT, color = Simulation), linewidth = 0.75) +
  scale_color_manual(values = cbPalette) +
  theme_minimal() +
  ylab("") +
  facet_wrap(~ year(Date), scales = "free_x") +
  theme(
    text = element_text(size = 15, family = 'serif'),
    plot.title = element_blank()
  )

# Combine panels
combined_plot <- (p1 + p2) / (p3 + p4) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ") ") & 
  theme(legend.position = "right")

# Add column labels using cowplot
title_row <- cowplot::plot_grid(
  cowplot::ggdraw() + 
    cowplot::draw_label("CARM", fontface = "bold", fontfamily = "serif", size = 16, hjust = 0.5),
  cowplot::ggdraw() + 
    cowplot::draw_label("TRM", fontface = "bold", fontfamily = "serif", size = 16, hjust = 0.45,
                        x = 0.3),
  ncol = 2
)

# Stack column labels above combined figure
final_plot <- cowplot::plot_grid(
  title_row,
  combined_plot,
  ncol = 1,
  rel_heights = c(0.05, 1)
)

# Display
print(final_plot)

# --- Publication Export (PNG at 300 dpi) ---
# Export plot
ggsave(
  filename = "C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/1605 weight gain/Figure_animal weight.png",
  plot = final_plot,
  width = 15,       # width in inches 
  height = 7,      # height in inches 
  dpi = 300,       # publication resolution
  units = "in",
  bg = "white"     # ensure transparent background is not used
)
