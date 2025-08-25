PastureID_sa92_soilComp <- read.csv("C:/Users/Sean.DiStefano/Documents/GitHub/APEX_spatial-temp-var/PastureID_ecosite_92subareas.csv")

# Prepare and plot
p1 <- PastureID_sa92_soilComp %>%
  count(Ecosite, apex_soilComp) %>%
  ggplot(aes(x = Ecosite, y = n, fill = apex_soilComp)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Ecological Site", y = "Frequency", fill = "Soil Type") +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 15)) +
  scale_fill_npg()


biomass_spatial_plot_soilComp <- biomass_summary_spatial_plot %>%
  left_join(PastureID_sa92_soilComp)

model_biomass_soil <- lmer(cumulative_DDMkg_ha ~ apex_soilComp * CPNM + Y + # fixed effects
                                (1 | Block), # random effect
                           biomass_spatial_plot_soilComp)

summary(model_biomass_soil)

emm_soil <- emmeans(model_biomass_soil, ~ apex_soilComp | CPNM)
pairs(emm_soil, adjust = "Tukey")

emm_soil <- as.data.frame(emm_soil)

p2 <- ggplot(emm_soil, aes(x = apex_soilComp, y = emmean, color = apex_soilComp)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ CPNM, scales = "free_y") +
  labs(y = "Mean Estimated Biomass (kg/ha)",
       x = "Soil Type") +
  theme_minimal() +
  scale_color_npg() +
  theme(text = element_text(family = "serif", size = 15),
        axis.text.x = element_blank()) +
  guides(color = "none")  

combined_plot <- p1 / p2 + 
  plot_annotation(tag_levels = 'a')

combined_plot

# Save combined plot as high-resolution PNG
ggsave("soil_type_panels.png",
       plot = combined_plot,
       width = 8, height = 10, units = "in",
       dpi = 300)
