library(ggplot2)

# Create the data frame
data <- data.frame(
  Ecosite = rep(c("Salt Flats", "Sandy", "Loamy"), each = 4),
  CPNM = factor(rep(c("WSPG", "CSPG", "FORB", "CSAG"), times = 3), levels = c("CSPG", "WSPG", "FORB", "CSAG")),
  RMSE = c(376.3536203, 273.645361, 170.4316596, 34.12089531,
           107.8102856, 307.4114116, 119.8791647, 227.9417475,
           189.1043681, 193.5929625, 171.2571292, 138.0010185),
  d = c(0.708670451, 0.796140718, 0.428294352, 0.880511323,
        0.860338037, 0.792299558, 0.671966345, 0.485461488,
        0.691777929, 0.646086748, 0.662792087, 0.774724728)
)


# Round d values to 2 decimal places
data$d <- round(data$d, 2)

# Plot the grouped bar chart for RMSE
ggplot(data, aes(x = Ecosite, y = RMSE, fill = CPNM)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(RMSE, 2)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 6, family = "Arial") +
  labs(title = "RMSE by Ecosite and CPNM Category", 
       x = "Ecosite", y = "RMSE", fill = "CPNM") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_family = "Arial") +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

# Plot the grouped bar chart for d values
ggplot(data, aes(x = Ecosite, y = d, fill = CPNM)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(d, 2)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 6, family = "Arial") +
  labs(title = "Willmott's Index of Agreement (Model Accuracy)", 
       x = "Ecological Site", y = "d (0-1)", fill = "Forage") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_family = "Arial") +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )


summarize_biomass_pasture <- function(data) {
  data %>%
    # Group data by treatment, ID, plant community (CPNM), and year
    group_by(Treatment, ID, CPNM, Y) %>%
    # Arrange data by date and calculate cumulative biomass for each observation
    arrange(Date) %>%
    mutate(cumulative_DDMkg_ha = cumsum(DDMkg_ha),
           month_day = format(Date, "%m-%d")) %>%
    ungroup() %>%
    # Group by date, treatment, pasture, plant community, year, and month-day
    group_by(Date, Treatment, Pasture, CPNM, Y, month_day) %>%
    # Calculate the mean cumulative biomass for each pasture
    summarize(mean_DDMkg_ha_pasture = mean(cumulative_DDMkg_ha, na.rm = TRUE)) %>%
    # Filter out specific plant community codes that are not of interest
    filter(!CPNM %in% c("ATCA", "SSHB")) %>%
    ungroup()
}

biomass_summary_no_var_pasture <- summarize_biomass_pasture(biomass_no_variability)

# Calculating 1 standard deviation within each Treatment across pastures
# - Calculate mean and standard deviation of observed data by treatment and pasture
observed_data_v04_pasture <- observed_data_v03 %>%
  group_by(Year, Date, Treatment, Pasture, APEXcodeFG) %>%
  summarize(MeankgPerHa_pasture = mean(MeankgPerHa_plot), .groups = 'drop',
            SDkgPerHa_pasture = sd(MeankgPerHa_plot, na.rm = TRUE)) %>%
  filter(Year <= 2018) %>%
  mutate(month_day = format(Date, "%m-%d"),
         Y = as.character(Year)) %>%
  rename(CPNM = APEXcodeFG) %>%
  filter(!CPNM %in% c("ATCA", "SSHB")) # plant groups not needed

biomass_summary_group <- biomass_summary_no_var_pasture %>% 
  filter(Pasture == "7NW") 
observed_data_group <- observed_data_v04_pasture %>% 
  filter(Pasture == "7NW")

observed_data_group$CPNM <- factor(observed_data_group$CPNM, 
                                   levels = c("CSPG", "WSPG", "FRB3", "VUOC"))

biomass_summary_group$CPNM <- factor(biomass_summary_group$CPNM, 
                                     levels = c("CSPG", "WSPG", "FRB3", "VUOC"))

# Rename levels
observed_data_group$CPNM <- fct_recode(observed_data_group$CPNM, 
                                       "FORB" = "FRB3", 
                                       "CSAG" = "VUOC")

biomass_summary_group$CPNM <- fct_recode(biomass_summary_group$CPNM, 
                                         "FORB" = "FRB3", 
                                         "CSAG" = "VUOC")


# Generate the plot for the current group
pasture7NW_biomass <- ggplot(biomass_summary_group, aes(x = month_day, y = mean_DDMkg_ha_pasture, 
                                                        group = Y)) +
  geom_line(linewidth = 1, aes(color = CPNM)) +
  geom_point(data = observed_data_group,
             aes(x = month_day, y = MeankgPerHa_pasture),
             size = 3, alpha = 1, stroke = 2, fill = NA) +
  scale_shape_manual(values = 1:2) +
  scale_color_npg() +
  scale_fill_npg() +
  geom_errorbar(data = observed_data_group,
                aes(x = month_day, y = MeankgPerHa_pasture, 
                    ymin = pmax(MeankgPerHa_pasture - SDkgPerHa_pasture, 0), 
                    ymax = MeankgPerHa_pasture + SDkgPerHa_pasture),
                linewidth = 1) +
  facet_grid(CPNM ~ Y, scales = "free_y") +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), 
                   labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Forage Type") +
  theme_minimal(base_family = "Arial") +
  theme(
    strip.text = element_text(size = 24, face = "bold", family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Arial", size = 24),
    axis.text.y = element_text(family = "Arial", size = 24),
    axis.title = element_text(family = "Arial", size = 26),
    legend.text = element_text(family = "Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 26),
    legend.position = "bottom"
  )

# Save the pasture plot
setwd("C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/1605 biomass")
ggsave(filename = "7NW_biomass_plots.png", plot = pasture7NW_biomass, width = 16, height = 12)

biomass_data <- biomass_summary_ecosite_pasture %>% filter(Pasture == "7NW")
observed_data <- observed_data_v04_ecosite_pasture %>% filter(Pasture == "7NW")
ecosite_type <- "Salt Flats"

observed_data$CPNM <- factor(observed_data$CPNM, 
                             levels = c("CSPG", "WSPG", "FRB3", "VUOC"))

biomass_data$CPNM <- factor(biomass_data$CPNM, 
                            levels = c("CSPG", "WSPG", "FRB3", "VUOC"))

ggplot(
  biomass_data %>% 
    filter(Ecosite == ecosite_type) %>%
    mutate(CPNM = factor(CPNM, levels = c("CSPG", "WSPG", "FRB3", "VUOC"))),
  aes(x = month_day, y = mean_DDMkg_ha_ecosite, group = Y)
) +
  geom_line(size = 1, aes(color = CPNM, group = CPNM)) +
  scale_color_npg() +
  facet_grid( ~ Y) +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"),
                   labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Forage Type") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 24, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24),
    axis.text.y = element_text(size = 24),
    axis.title = element_text(size = 26),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 26),
    legend.position = "bottom"
  )
# Define a function to summarize biomass data by ecological site
# - Summarizes cumulative biomass within each ecological site
summarize_biomass_ecosite_pasture <- function(data) {
  data %>%
    group_by(Pasture, Ecosite, ID, CPNM, Y) %>%
    arrange(Date) %>%
    mutate(
      cumulative_DDMkg_ha = cumsum(DDMkg_ha),
      month_day = format(Date, "%m-%d")
    ) %>%
    ungroup() %>%
    group_by(Date, Pasture, Ecosite, CPNM, Y, month_day) %>%
    summarize(
      mean_DDMkg_ha_ecosite = mean(cumulative_DDMkg_ha, na.rm = TRUE),
      sd_DDMkg_ha_ecosite = sd(cumulative_DDMkg_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!CPNM %in% c("ATCA", "SSHB"))
}

# Summarize biomass by ecological site
biomass_summary_ecosite_pasture <- summarize_biomass_ecosite_pasture(biomass_spatial)

# Calculate 1 standard deviation within each Treatment across ecosites
observed_data_v04_ecosite_pasture <- observed_data_v03 %>%
  group_by(Year, Date, Pasture, Ecosite, APEXcodeFG) %>%
  summarize(MeankgPerHa_ecosite = mean(MeankgPerHa_plot), 
            SDkgPerHa_ecosite = sd(MeankgPerHa_plot, na.rm = TRUE),
            .groups = 'drop') %>%
  filter(Year <= 2018) %>%
  mutate(month_day = format(Date, "%m-%d"),
         Y = as.character(Year)) %>%
  rename(CPNM = APEXcodeFG) %>%
  filter(!CPNM %in% c("ATCA", "SSHB"))

