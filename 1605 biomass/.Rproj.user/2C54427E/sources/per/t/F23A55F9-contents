library(patchwork)
library(ggsci)

## Functional Group Biomass
my_colors <- c("orangered2", "gray36")

func.group <- ggplot() +
  geom_ribbon(aes(x = Date, ymin = Mean - SD, ymax = Mean + SD),
              subset(Biomass_base_graze, Type %in% "Simulated"),
              alpha = 0.15) +
  geom_line(aes(x = Date, y = Mean, color = Treatment),
            subset(Biomass_base_graze, Type %in% "Simulated"),
            linewidth = 1, alpha = 0.6) +
  geom_point(aes(x = Date, y = Mean, 
                 color = Treatment, shape = Treatment),
             subset(Biomass_base_graze, Type == "Observed"),
             size = 3) +
  geom_errorbar(aes(x = Date, 
                    ymin = Mean - SD, ymax = Mean + SD,
                    color = Treatment),
                subset(Biomass_base_graze, Type %in% "Observed"),
                linewidth = 0.7, width = 100) +
  facet_wrap(~factor(APEXcodeFG, 
                     levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")), 
             ncol = 1, scales = "free_y",
             ) +
  ylab(expression(paste("Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  theme_bw() +
  theme(text = element_text(size = 12.5, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  scale_shape_manual(values = c(17, 1)) +
  scale_color_manual(values = my_colors)

## Total Biomass
# cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")

tot.bm <- ggplot() +
  geom_ribbon(aes(x = Date,
                  ymin = Total_Mean - SD, ymax = Total_Mean + SD),
              subset(Biomass_trt_grazeTotal, Type %in% "Simulated"),
              alpha = 0.15) +
  geom_line(aes(x = Date, y = Total_Mean, color = Treatment),
            subset(Biomass_trt_grazeTotal, Type %in% "Simulated"),
            linewidth = 1, alpha = 0.6) +
  geom_point(aes(x = Date, y = Total_Mean, 
                 color = Treatment, shape = Treatment),
             subset(Biomass_trt_grazeTotal, Type == "Observed"),
             size = 4) +
  geom_errorbar(aes(x = Date, 
                    ymin = Total_Mean - SD, 
                    ymax = Total_Mean + SD,
                    color = Treatment),
                subset(Biomass_trt_grazeTotal, Type %in% "Observed"),
                linewidth = 1, width = 100) +
  ylab(expression(paste("Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  theme_bw() +
  theme(text = element_text(size = 12.5, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines")) +
  scale_shape_manual(values = c(17, 1)) +
  scale_color_manual(values = my_colors)

## Daily precipitation data from Nicole Kaplan
cper_climate <- read.csv("D:/APEX data and scripts/Data/CPER PPT/CPER daily climate data_model input.csv") %>%
  filter(year >= 2014) %>%
  mutate(Date = ymd(paste(year, month, day, sep = "-"))) %>%
  select(Date, rain.mm., Tmax.oC., Tmin.oC.) %>%
  mutate(daily_temp = (Tmax.oC. + Tmin.oC.) / 2) %>%
  rename(daily_precip = rain.mm.)

min.temp <- min(cper_climate$daily_temp)

coeff <- 20 / min.temp

cper_climate$daily_temp_scale <- cper_climate$daily_temp * coeff

precip <- ggplot(cper_climate) + 
  geom_bar(aes(x = Date, y = daily_precip),
           alpha = 0.3,
           stat = 'identity',
           color = "steelblue") +
  theme_bw() +
  theme(text = element_text(size = 12.5, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.75, "lines")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10)) +
  ylab("Precipitation (mm)") 

temp <- ggplot(cper_climate) +
  geom_line(aes(x = Date, y = daily_temp),
            color = "maroon4", alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(size = 12.5, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.75, "lines")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10)) +
  ylab(expression("Mean Air Temp." (degree~C)))

my_colors <- c("dodgerblue2", "gold")
  
plant.stress <- ggplot(data = SimBiom_plantStress) +
  geom_line(aes(x = Date, y = WS_mean, color = "Water")) +
  geom_line(aes(x = Date, y = TS_mean, color = "Temperature"),
            linetype = "dashed",
            linewidth = 0.5, alpha = 0.6) +
  facet_wrap(~factor(CPNM, 
                     levels = c("WSPG", "CSPG", "FORB", "SSHB", "CSAG")), 
             ncol = 1) +
  ylab(expression(paste("Plant Stress (0-1)"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  theme_bw() +
  theme(text = element_text(size = 12.5, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  labs(color = "Plant Stress") +
  scale_color_manual(values = my_colors)

## Creating combined plot
(precip | temp) / tot.bm / (func.group | plant.stress) +
  plot_layout(heights = c(0.5, 0.5, 2))

# exporting plot
# png("C:/Users/sean.di_stefano/OneDrive - USDA/Documents/GitHub/APEX_spatial-temp-var/1605 biomass/biomass_prod_ggplot.png")
# print(bm_plot, width = 10000, height = 40000)
# dev.off()
