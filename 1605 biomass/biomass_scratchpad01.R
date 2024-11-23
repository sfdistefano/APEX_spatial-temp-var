# Summarize the data to reduce processing time for plotting and accumulate data over time
biomass_summary <- biomass_cageSim_spatVar_v02 %>%
  group_by(Treatment, ID, CPNM, Y) %>%  # Group by Treatment, CPNM, and Year
  arrange(Date) %>%
  mutate(cumulative_DDMkg_ha = cumsum(DDMkg_ha),
         month_day = format(Date, "%m-%d")) %>%  # Accumulate DDM for each day within each year and create a month_day column
  ungroup() %>%
  group_by(Date, Treatment, Pasture, CPNM, Y, month_day) %>%
  summarize(mean_DDMkg_ha_pasture = mean(cumulative_DDMkg_ha, na.rm = TRUE)) %>%
  group_by(Date, Treatment, CPNM, Y, month_day) %>%
  summarize(mean_DDMkg_ha = mean(mean_DDMkg_ha_pasture),
            sd_DDMkg_ha = sd(mean_DDMkg_ha_pasture, na.rm = TRUE)) %>%
  ungroup()

# Create ggplot object to visualize accumulated biomass (DDMkg_ha) by Treatment for each CPNM and Year combination
biomass_plot <- ggplot(biomass_summary, aes(x = month_day, y = mean_DDMkg_ha, color = Treatment, group = Treatment)) +
  geom_ribbon(aes(ymin = pmax(mean_DDMkg_ha - sd_DDMkg_ha, 0), ymax = mean_DDMkg_ha + sd_DDMkg_ha, fill = Treatment), alpha = 0.2) +
  geom_line(size = 1) +
  facet_grid(CPNM ~ Y, scales = "free_y") +
  scale_x_discrete(breaks = c("01-01", "03-01", "05-01", "07-01", "09-01", "11-01"), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  labs(title = "Accumulated Biomass Across Treatments, Years, and Plant Functional Groups",
       x = "Month-Day",
       y = "Accumulated Biomass (kg/ha)",
       color = "Grazing Treatment",
       fill = "Grazing Treatment") +
  theme_minimal(base_family = "serif") +
  theme(
    strip.text = element_text(size = 10, face = "bold", family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "serif"),
    legend.position = "bottom"
  )

# Display the plot
print(biomass_plot)

# Save the plot to a .png file
ggsave(filename = "acc_biomass_plot.png", plot = biomass_plot, width = 12, height = 8, dpi = 300)
