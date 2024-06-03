wspg <- veg.list[[1]] %>% filter(Ecosite == "Mixed")

intercept0 <- (y ~ 0 + x)

ggplot(wspg, aes(x = biomass, y = basal.area, color = Ecosite)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE, formula = intercept0) +
  stat_regline_equation(formula = intercept0, 
                        label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(formula = intercept0,
                        label.y = 10, aes(label = ..rr.label..))
