# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(patchwork)
library(emmeans)
library(ggsci)

####### PANEL A: Monthly Precipitation #######

# Set column names
column_names <- c("year", "month", "day", "Srad", "Tmax", "Tmin", "rain_mm", "RH", "windspd", "Date")

# Read the daily weather data
weather_data <- read.csv("C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/CPER_climate data.csv") %>%
  filter(year %in% c(2014:2018),
         month %in% c(5:10)
  ) %>%
  mutate(Date = ymd(paste(year,month,day, sep = "-")))

# Assigning new column names
colnames(weather_data) <- column_names

# Monthly precipitation
monthly_precip <- weather_data %>%
  group_by(year, month) %>%
  summarize(total_rain = sum(rain_mm, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_name = factor(month, levels = 1:12, labels = month.abb))

# Annual precipitation totals
annual_precip <- weather_data %>%
  group_by(year) %>%
  summarize(yearly_rain = sum(rain_mm, na.rm = TRUE))

# Create label positions
annual_precip <- annual_precip %>%
  mutate(Date = as.Date(paste0(year, "-10-15")))  # e.g., place label in mid October

# Plot Panel A
plot_precip <- ggplot(weather_data, aes(x = Date, y = rain_mm)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Month", y = "Daily Precipitation (mm)") +
  geom_text(data = annual_precip, 
            aes(x = Date, y = Inf, label = paste0("Total: ", round(yearly_rain, 0), " mm")), 
            vjust = 1.2, family = "serif", size = 4, inherit.aes = FALSE) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold", family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "serif", size = 12),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.title = element_text(family = "serif", size = 14))


####### PANEL B: Monthly Tmax and Tmin #######

# Plot Panel B
plot_temp <- ggplot(weather_data, aes(x = Date)) +
  geom_line(aes(y = Tmax, color = "Max Temp"), linewidth = 1.2) +
  geom_line(aes(y = Tmin, color = "Min Temp"), linewidth = 1.2) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +  
  scale_color_manual(values = c("Max Temp" = "#D55E00", "Min Temp" = "#0072B2")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  
  labs(x = "Month", y = "Air Temperature (°C)", color = "Daily Temperature") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold", family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "serif", size = 12),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.title = element_text(family = "serif", size = 14),
        legend.text = element_text(family = "serif", size = 12),
        legend.title = element_text(family = "serif", size = 14),
        legend.position = "bottom")

####### PANEL C & D: Plant Stress (by CPNM) #######
# Function to read and preprocess plant stress
read_preprocess_stress <- function(file_path, metadata_path, skip_lines = 9, years = 2014:2018) {
  plantStress_data <- fread(file_path, fill = TRUE, skip = skip_lines, header = TRUE) %>%
    select(ID, Y, M, D, CPNM, WS, TS) %>%
    filter(Y %in% years) %>%
    mutate(Date = ymd(paste(Y, M, D, sep = "-")),
           ID = as.integer(ID)) 
  
  metadata <- read.csv(metadata_path)
  
  left_join(plantStress_data, metadata, by = "ID")
}

# Summarization function (corrected to handle dates)
summarize_stress_cpnm <- function(data, stress_var) {
  data %>%
    mutate(month_day = as.Date(format(Date, "2000-%m-%d"))) %>%  # use dummy year for plotting
    group_by(Date, Pasture, CPNM, Y, month_day) %>%
    summarize(stress_pasturePlant = mean(.data[[stress_var]], na.rm = TRUE), .groups = "drop") %>%
    group_by(Date, Y, CPNM, month_day) %>%
    summarize(cpnm_stress_daily = round(mean(stress_pasturePlant ), 2),
              sd_cpnm_stress_daily = round(sd(stress_pasturePlant ), 2), .groups = "drop")
}

# Read data (spatial & no variability)
spatial <- read_preprocess_stress(
  file_path = "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all92/CONUNN_TGM.cag",
  metadata_path = "C:/APEX data and scripts/Data/PastureID_ecosite_92subareas.csv"
)

no_variability <- read_preprocess_stress(
  file_path = "C:/Users/Sean.DiStefano/Downloads/APEX1605_NEW/APEX1605_CO_all20/CONUNN_TGM.cag",
  metadata_path = "C:/APEX data and scripts/Data/PastureID_ecosite_20subareas.csv"
)

# Summarize both stress types
spatial_WS_CPNM <- summarize_stress_cpnm(spatial, "WS") %>% mutate(Sim.Type = "Spatial Variability")
noVar_WS_CPNM <- summarize_stress_cpnm(no_variability, "WS") %>% mutate(Sim.Type = "No Variability")
combined_WS_CPNM <- bind_rows(spatial_WS_CPNM, noVar_WS_CPNM) %>%
  filter(month_day >= as.Date("2000-05-01") & month_day <= as.Date("2000-10-31"))

spatial_TS_CPNM <- summarize_stress_cpnm(spatial, "TS") %>% mutate(Sim.Type = "Spatial Variability")
noVar_TS_CPNM <- summarize_stress_cpnm(no_variability, "TS") %>% mutate(Sim.Type = "No Variability")
combined_TS_CPNM <- bind_rows(spatial_TS_CPNM, noVar_TS_CPNM) %>%
  filter(month_day >= as.Date("2000-05-01") & month_day <= as.Date("2000-10-31"))

model_ws <- lm(cpnm_stress_daily ~ Sim.Type * CPNM + Y, data = combined_WS_CPNM)
model_ts <- lm(cpnm_stress_daily ~ Sim.Type * CPNM + Y, data = combined_TS_CPNM)

summary(model_ws)
summary(model_ts)

# Get estimated marginal means from each model
emm_ws <- emmeans(model_ws, ~ Sim.Type | CPNM) 

emm_ws_df <- emm_ws %>%
  as.data.frame() %>%
  mutate(StressType = "Water Stress")

# Post-hoc analysis of pair-wise contrasts
pairs(emm_ws, adjust = "tukey", reverse = TRUE)

emm_ts <- emmeans(model_ts, ~ Sim.Type | CPNM) 

emm_ts_df <- emm_ts %>%
  as.data.frame() %>%
  mutate(StressType = "Temperature Stress")

# Post-hoc analysis of pair-wise contrasts
pairs(emm_ts, adjust = "tukey", reverse = TRUE)

# Combine
emm_combined <- bind_rows(emm_ws_df, emm_ts_df) %>%
  mutate(CPNM = recode(CPNM,
                       "VUOC" = "CSAG",
                       "FRB3" = "FORB")) %>%
  mutate(CPNM = fct_relevel(CPNM, "CSPG", "WSPG", "FORB", "CSAG"))

# Plot
plot_stress <- ggplot(emm_combined, aes(x = CPNM, y = emmean, fill = Sim.Type)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(0.7), width = 0.4) +
  facet_wrap(~ StressType) +
  labs(y = "Mean Daily Stress (0 = max stress, 1 = no stress)",
       x = "Plant Group", fill = "Simulation Type") +
  scale_fill_npg() +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Overall means
# mean_WS_cpnm <- combined_WS_CPNM %>%
#   group_by(CPNM, Sim.Type) %>%
#   summarize(ws_cpnm = round(mean(cpnm_stress_daily), 2))
# 
# mean_TS_cpnm <- combined_TS_CPNM %>%
#   group_by(CPNM, Sim.Type) %>%
#   summarize(ts_cpnm = round(mean(cpnm_stress_daily), 2))

####### Assemble Final 2x2 Layout #######

final_plot <- ((plot_precip + plot_temp) / plot_stress) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 18, face = "bold", family = "serif"))

# Show plot
final_plot

####### Export Figure #######

ggsave("Figure_Climate_Stress.tiff", plot = final_plot,
      width = 16, height = 16, dpi = 300, compression = "lzw")
