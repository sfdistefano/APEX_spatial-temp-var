library(patchwork)

## Functional Group Biomass
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

func.group <- ggplot() +
  geom_ribbon(aes(x = Date, ymin = Mean - SD, ymax = Mean + SD),
              subset(Biomass_base_graze, Type %in% "Simulated"),
              alpha = 0.15) +
  geom_line(aes(x = Date, y = Mean, color = Sim.Type),
            subset(Biomass_comb_graze, Type %in% "Simulated"),
            linewidth = 1) +
  geom_point(aes(x = Date, y = Mean, color = "Observed"),
             subset(Biomass_base_graze, Type == "Observed"),
             size = 1.75, stroke = 0.7) +
  geom_errorbar(aes(x = Date, ymin = Mean - SD, ymax = Mean + SD,
                    color = "Observed"),
                subset(Biomass_base_graze, Type %in% "Observed"),
                linewidth = 0.7, width = 100) +
  facet_grid(APEXcodeFG ~ Treatment,
             scales = "free_y") +
  ylab(expression(paste("Standing Biomass (kg ", " ha" ^-1,")"))) +
  xlab("Date (Month-Year)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))+
  scale_color_manual(values = cbbPalette,
                     name = "Forage Production Data") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        panel.spacing = unit(0.75, "lines"))

## Total Biomass
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")

tot.bm <- ggplot(data = Biomass_comb_totbm) +
  geom_ribbon(aes(x = Date, ymin = tot_bm_trt - SD, ymax = tot_bm_trt + SD),
              subset(Biomass_comb_totbm, Sim.Type %in% "Simulated: no variability"),
              alpha = 0.15) +
  geom_line(aes(x = Date, y = tot_bm_trt, color = Sim.Type),
            linewidth = 1) +
  geom_point(data = obv22_trt,
             aes(x = Date, y = tot_bm_trt),
             size = 1.75, stroke = 0.7) +
  geom_errorbar(data = obv22_trt,
                aes(x = Date, ymin = tot_bm_trt - SD, ymax = tot_bm_trt + SD),
                linewidth = 0.7, width = 100) +
  facet_grid(~Treatment) +
  scale_color_manual(values = cbbPalette,
                     guide = "none") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.75, "lines")) +
  ylab(expression(paste("Standing Biomass (kg ", " ha" ^-1,")"))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10))

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
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.75, "lines")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10)) +
  ylab("Precipitation (mm)") 

temp <- ggplot(cper_climate) +
  geom_line(aes(x = Date, y = daily_temp),
            color = "black") +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'serif'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.75, "lines")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%y",expand = c(0,10)) +
  ylab("Mean Air Temp. (C)")

## Creating combined plot
(precip | temp) / tot.bm / func.group +
  plot_layout(heights = c(0.75, 0.5, 2)) 

# exporting plot
# png("C:/Users/sean.di_stefano/OneDrive - USDA/Documents/GitHub/APEX_spatial-temp-var/1605 biomass/biomass_prod_ggplot.png")
# print(bm_plot, width = 10000, height = 40000)
# dev.off()
