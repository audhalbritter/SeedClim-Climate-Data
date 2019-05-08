#### soil moisture ####
library(readxl)
library(tidyverse)
library(lubridate)
library(lme4)
library(broom)

#seedclim site-level soil moisture
#load("/Volumes/fja062/PhD/Projects/2017_temperature_regulation_of_functional_groups/SeedClim-Climate-Data/Soilmoisture.RData")

#use soil moisture differences!
# read in soil moisture data FUNCAB point measurements
SM201516 <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/primary/climate_data/soilMoisture_2015-2016.xlsx")
#SM2017 <- read_excel(path = "/Volumes/fja062/PhD/Data/Soilmoisture2017.xlsx")

SM201516 <- SM201516 %>% 
  mutate_at(.vars = c("M1", "M2", "M3", "M4"), as.numeric) %>% 
  filter(comments %in% c("fewer readings", "above table", "above table 100%")|is.na(comments)) %>%
  filter(!grepl("TT1", turfID),
         !grepl("TT2", turfID),
         !grepl("TT3", turfID),
         !grepl("TT4", turfID),
         !grepl("RTC", turfID),
         !grepl("P", turfID),
         !is.na(treatment)) %>% 
  group_by(date, turfID) %>% 
  mutate(sameDayMeasurement = n()) %>% 
  #filter(n > 1) %>% 
  mutate(SM = mean(c(M1, M2, M3, M4), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(siteID = plyr::mapvalues(site, from = dict_Site$old, to = dict_Site$new)) %>%
  mutate(FCturfID = if_else(!is.na(removal), paste0(str_sub(siteID, 1, 3), block, removal), ""), date = ymd(date)) %>%
  filter(!treatment == "XC") %>% 
  #!weather %in% c("raining, still", "raining", "Raining", "cloudy/raining", "Overcast; Rainy", "overcast, rainy")) %>%
  select(-site, -c(M1:M4), -blockSD, -treatment, -comments, -Moisture) %>% 
  rename(Treatment = removal) %>% 
  select(-sameDayMeasurement)

SoilMoisture <- SM201516 %>% 
  filter(!is.na(block)) %>%  
  select(date, siteID, turfID, Treatment, "blockID" = block, weather, SM)

cntrls <- SoilMoisture %>% right_join(dict_TTC_turf, by = c(turfID = "TTtreat"), suffix = c("", ".new")) %>% 
  select(date, siteID, "turfID" = turfID.new, Treatment, blockID, weather, SM)

SoilMoisture <- SoilMoisture %>%
  filter(!grepl("TTC", turfID)) %>% 
  bind_rows(cntrls)


smVeg <- vegComp %>% 
  filter(between(date, ymd("2015-05-09"), ymd("2016-08-29")), !Treatment == "temp200cm") %>%
  distinct(siteID, date, blockID, Treatment, turfID, sunniness, meanTemp, vegetationHeight, mossHeight, mossCov, forbCov, graminoidCov) %>%
  left_join(SoilMoisture) %>% 
  filter(!is.na(SM), !Treatment == "XC")  %>% 
  left_join(weather)


smVegAnom <- smVeg %>% 
  left_join(smVeg %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBSM = SM, date, siteID, blockID)) %>%
  mutate(SMAnom = SM - FGBSM) %>% 
  ungroup()

smVegAnom <- smVegAnom %>%
  filter(!Treatment == "FGB") %>% 
  group_by(siteID, blockID, turfID, Treatment, forbCov, graminoidCov, mossCov, vegetationHeight, mossHeight) %>% 
  summarise(meanSMAnom = mean(SMAnom)) %>%
  left_join(weather) 



#### Analysis ####
# treatment model

smVegAnalysis <- smVeg %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB"),
         sTemp70 = scale(temp7010),
         sPrecip70 = scale(precip7010)) %>%
  filter(Treatment %in% c("FB", "GB", "GF", "C", "aFGB"))

modSMT <- lmer(SM ~ Treatment*sTemp70*sPrecip70 +  (1|siteID/blockID), REML = TRUE, data = smVegAnalysis)

modSMTt <- modSMT %>% 
  tidy() %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

coefPlotsmT <- modSMTt %>%
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("sPrecip70", "P", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("TreatmentFB", "G", term),
         term = gsub("TreatmentGB", "F", term),
         term = gsub("TreatmentGF", "B", term),
         term = gsub("TreatmentC", "C", term),
         term = gsub(":", " x ", term)
  ) %>% 
  mutate(term3 = case_when(
    grepl("G", term) ~ "Graminoids",
    grepl("F", term) ~ "Forbs",
    grepl("B", term) ~ "Bryophytes",
    grepl("C", term) ~ "Intact vegetation",
    term %in% c("t", "P") ~ "Climate"
  ))

modSMTplot <- coefPlotsmT %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, shape = term3)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept =  c(2.5, 6.5, 10.5), colour = pal1[c(5,2,4)], size = 31.5, alpha = 0.2) +
  geom_vline(xintercept = 13.5, colour = "grey", size = 15.75, alpha = 0.2) +
  geom_point(position = position_dodge(width = 0.1), size = 3, fill = "grey") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) + 
  scale_shape_manual("", values = c(21,22,23,24,25), limits = c("Graminoids", "Forbs", "Bryophytes", "Intact vegetation", "Climate")) + 
  scale_x_discrete(limits = c("C x t x P", "B x t x P", "F x t x P", "G x t x P",
                              "C x P",  "B x P", "F x P", "G x P",
                              "C x t", "B x t", "F x t", "G x t",
                              "t", "P", "C", "B", "F", "G")) +
  coord_flip() +
  ylim(-13,16) +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme_classic() +
  axis.dimLarge +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

ggsave(modSMTplot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig5.jpg", dpi = 300, width = 5.1, height = 5.4)

SMmod <- coefPlotsmT %>% 
  filter(!is.na(term)) %>% 
  mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), statistic = round(statistic, 3))
write_excel_csv(SMmod, path = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/data/soilMoisMod.csv")


#### Figures ####

SMplot <- smVegAnom %>%
  ggplot(aes(x = Treatment, y = meanSMAnom, colour = Treatment, fill = Treatment)) + 
  geom_boxplot() +
  #stat_summary(fun.data = "mean_cl_boot") +
  #stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 100), geom = "line") +
  geom_hline(yintercept = 0) + 
  scale_colour_manual("Vegetation", values = cbPalette[c(10, 1, 4, 6, 2, 5, 9)], labels = c("Forbs and graminoids", "Forbs, graminoids and bryophytes", "Graminoids and bryophytes", "Graminoids", "Bryophytes and forbs", "Forbs","Bryophytes")) +
  scale_fill_manual("Vegetation", values = cbPalette[c(10, 1, 4, 6, 2, 5, 9)], labels = c("Forbs and graminoids", "Forbs, graminoids and bryophytes", "Graminoids and bryophytes", "Graminoids", "Bryophytes and forbs", "Forbs","Bryophytes")) +
  facet_wrap(~precipLevel)


# supplementary figure 10
smVeg %>% 
  ggplot(aes(x = SM)) + 
  geom_density(fill = "goldenrod", alpha = 0.3) + 
  geom_rug() + facet_grid(precipLevel~Treatment) + 
  labs(x = "Soil moisture")

ggsave(file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig10.jpg", dpi = 300, width = 10, height = 5)

