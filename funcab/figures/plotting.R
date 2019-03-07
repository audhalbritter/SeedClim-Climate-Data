source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/plotting_dim.R")

library(tidyverse)
library(cowplot)
library(wesanderson)
#################### 
##### plotting ##### 

soilTemp %>%
  filter(Treatment %in% c("F", "G", "B", "FGB", "C"), date > "2015-07-01", date < "2015-09-01") %>% 
  ggplot(aes(x = hour, y = Value, colour = Treatment)) +
  geom_smooth(se = FALSE) +
  #stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_grid(. ~ Temperature_level) +
  scale_color_manual(values = cbPalette[c(1, 3, 8, 4, 7)], labels = c("Control", "Bryophyte", "Forb", "Graminoid", "All"), name = "Removal") +
  labs(x = "Time (hr)", y = "Soil temperature (ºC)") +
  theme_classic() +
  axis.dimLarge 


soilTemp %>%
  mutate(weather = case_when(
    sunniness > 0.66 ~ "sunny",
    sunniness > 0.33 ~ "variable",
    sunniness < 0.33 ~ "cloudy")) %>% 
  filter(Treatment %in% c("F", "G", "B", "FGB", "C"), weather %in% c("sunny", "cloudy"), between(date, ymd("2015-07-01"), ymd("2015-09-01"))) %>% 
  ggplot(aes(x = hour, y = Value, colour = Treatment, linetype = weather)) +
  geom_smooth(se = FALSE) +
  #stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_grid(Precipitation_level ~ Temperature_level) +
  scale_color_manual(values = cbPalette[c(1, 3, 8, 4, 7)], labels = c( "Control", "Bryophyte", "Forb", "Graminoid", "All"), name = "Removal") +
  labs(x = "Time (hr)", y = "Soil temperature (ºC)") +
  theme_classic() +
  axis.dimLarge 


soilTemp %>%
  filter(Treatment %in% c("C", "FGB", "B", "G", "F")) %>% 
  filter(date > "2015-08-01", date < "2016-08-01") %>% 
  ggplot(aes(x = date, y = Value, colour = Treatment)) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_grid(Precipitation_level ~ Temperature_level) +
  scale_color_manual(values = cbPalette[c(1, 3, 4, 8, 7)], labels = c("Control", "Bryophyte", "Graminoid", "Forb", "All"), name = "Removal") +
  labs(y = "Soil temperature (ºC)") +
  theme_classic() +
  axis.dimLarge + 
  theme(axis.text.x  = element_text(angle = 45),
        axis.title.x = element_blank()) 



############################ 
#### Figure for article ####

alpine <- soilTemp %>%
  mutate(weather = case_when(sunniness > 0.66 ~ "sunny",
                             sunniness > 0.33 ~ "variable",
                             sunniness < 0.33 ~ "cloudy")) %>% 
  filter( weather %in% c("sunny", "cloudy"), between(date, ymd("2015-08-01"), ymd("2015-09-01")), Temperature_level == 6.5) %>% 
  ggplot(aes(x = hour, y = Value, colour = Treatment, linetype = weather)) +
  geom_smooth(se = FALSE, method = "loess") +
  scale_colour_manual("Functional groups", values = c("black", cbPalette[c(7,5,6,2,4,3,1)]), limits = c("FGB","GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bare ground","Bryophytes", "Forbs","Graminoids", "Bryophytes and forbs", "Graminoids and bryophytes", "Forbs and graminoids", "Bryophytes, forbs and\ngraminoids")) +
  labs(x = "Time (hr)", y = "Soil temperature (ºC)") +
  #scale_linetype_manual(values = c("73",1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  #ylim(7,19) +
  axis.dimLarge

  
  # approx(x = dateTime, y = temp, xout = seq(midnight), ) to interpolate which gives list of x and y %>% as.dataframe() %>% rename() and add missing cols mutate()
FD <- soilTemp %>%
  ungroup() %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-29")), !Treatment == "temp200cm", Temperature_level == 6.5, !is.na(Value)) %>%
  #filter(!turfID %in% c("Lav1GF", "Lav4GF", "Lav2GF")) %>% 
  group_by(date, Treatment, siteID) %>% 
  summarise(minTemp = min(Value)) %>% 
  mutate(x = minTemp < 0) %>%
  arrange(date) %>% 
  group_by(Treatment, siteID) %>% 
  mutate(sum = cumsum(x), n = n()) %>%
  ungroup()

x <- FD %>% 
  ggplot(aes(x = date, y = sum, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line", size = 0.8) +
  scale_x_discrete(limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C"), labels = c("bare","B", "F", "G", "FB", "GB", "GF", "FGB")) +
  scale_colour_manual("Vegetation", values = c("black", cbPalette[c(7,5,6,2,4,3,1)]), limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C")) +
  scale_fill_manual("Vegetation", values = c("black", cbPalette[c(7,5,6,2,4,3,1)]), limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C")) +
  coord_cartesian(ylim = c(0, 85)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  axis.dimLarge +
  ylab("Cumulative frost\ndays") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks=element_blank(),
        axis.line.x = element_blank(),
        axis.title=element_text(size=15))

y <- soilTemp %>%
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-29")), Temperature_level == 6.5, !Treatment == "temp200cm") %>% 
  #mutate(Treatment = factor(Treatment, levels = c("C", "FB", "GB", "GF", "FGB"))) %>% 
  group_by(Treatment, date) %>% 
  mutate(meanTemp = mean(Value)) %>% 
  ggplot(aes(x = date, y = meanTemp, colour = Treatment)) +
  geom_line(size = 0.8, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual("Functional groups", values = c("black", cbPalette[c(7,5,6,2,4,3,1)]), limits = c("FGB","GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bare ground","Bryophytes", "Forbs","Graminoids", "Bryophytes & forbs", "Graminoids & bryophytes", "Forbs & graminoids", "Bryophytes, forbs &\ngraminoids")) +
  ylab("Soil temperature (°C)") +
  axis.dimLarge +
  theme(axis.title=element_text(size=15),
        axis.title.x = element_blank())

legend <- get_legend(alpine)
z <- plot_grid(y + theme(legend.position = "none"), x, ncol = 1, rel_heights = c(1.65, 1), align = "hv", labels = c("B", "C"), label_x = 0.055, label_y = 0.97)
#eps <- plot_grid(legend, lav, ncol = 1, rel_heights = c(1,1.65))

frostSumAnn <- plot_grid(alpine + theme(legend.position = "none"), z, legend, nrow = 1, rel_widths = c(0.95, 1, 0.49), labels = "A", label_x = 0.02, label_y =0.97)
ggsave(frostSumAnn, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig1.jpg", dpi = 300, width = 12.5, height = 5)


soilTemp %>% filter(format(Date, "%Y-%m") == "2015-09") %>%
  filter(Treatment %in% c("C","FGB", "2m_site")) %>%
  mutate(hour = hour(Date)) %>% 
  group_by(Treatment, iButtonID, hour, Temperature_level, Precipitation_level) %>%
  summarise(mean = mean(Value)) %>%
  ggplot(aes(x = hour, y = mean, colour = Treatment)) +
  geom_smooth(se = TRUE) +
  facet_grid(Precipitation_level ~ Temperature_level) +
  scale_color_manual(values = cbPalette) +
  theme_classic()

soilTemp %>% 
  filter(!Treatment == "2m_site") %>% 
  filter(!is.na(Treatment)) %>% 
  ggplot(aes(x = Date, y = Value, colour = Treatment)) +
  geom_line() +
  facet_wrap(~ siteID)

soilTemp %>% 
  filter(siteID == "Lavisdalen", Treatment == "GF") %>%
  filter(date < "2016-07-15" & Year == 2016) %>%
  filter(date > "2016-06-20") %>% 
  ggplot(aes(x = date, y = Value, colour = Block)) +
  geom_line()


# and we need to add figures to the .gitignore
