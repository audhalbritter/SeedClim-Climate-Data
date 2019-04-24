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
SM201516 <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/climate_data/soilMoisture_2015-2016.xlsx")
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

save(SM201516, file = "~/OneDrive - University of Bergen/Research/FunCaB/Data/soilMoisture.RData")


SoilMoisture <- SM201516 %>% 
  filter(!is.na(block)) %>% 
  mutate(Treatment = if_else(Treatment == "TTC", "C", Treatment)) %>% 
  select(date, siteID, turfID, Treatment, "blockID" = block, weather, SM) %>%
  mutate(turfID = plyr::mapvalues(turfID, from = dict_TTC_turf$TTtreat, to = dict_TTC_turf$turfID))


smVeg <- vegComp %>% 
  filter(between(date, ymd("2015-05-09"), ymd("2016-08-29")), !Treatment == "temp200cm") %>%
  distinct(siteID, date, blockID, Treatment, turfID, sunniness, meanTemp, vegetationHeight, mossHeight, litter, mossCov, forbCov, graminoidCov, tempLevel, precipLevel) %>% #, gridPrecipitation
  left_join(SoilMoisture) %>% 
  filter(!is.na(SM))
  #mutate(Treatment = relevel(as.factor(Treatment), ref = "C")) %>% 
  arrange(date) %>% 
  group_by(turfID) %>% 
  mutate(lagPrec = lag(gridPrecipitation, n = 1)) %>% 
  #filter(!(lagPrec > 7)) %>% 
  ungroup() %>% 
  distinct(siteID, Block, turfID, date, Treatment, bryophyteCov, mossHeight, forb, forbCov, graminoidCov, graminoid, SM, Precip, Temp, vegetationHeight, T_level, P_level)


smVegAnom <- smVeg %>% 
  left_join(smVeg %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBSM = SM, date, siteID, blockID)) %>%
  mutate(SMAnom = SM - FGBSM) %>% 
  ungroup()

SMplot <- smVegAnom %>%
  filter(!Treatment == "FGB") %>% 
  ggplot(aes(x = precipLevel, y = SMAnom, colour = Treatment, fill = Treatment)) + 
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 100)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 100), geom = "line") +
  geom_hline(yintercept = 0) + 
  scale_colour_manual("Vegetation", values = cbPalette[c(10, 1, 4, 6, 2, 5, 9)], labels = c("Forbs and graminoids", "Forbs, graminoids and bryophytes", "Graminoids and bryophytes", "Graminoids", "Bryophytes and forbs", "Forbs","Bryophytes")) +
  scale_fill_manual("Vegetation", values = cbPalette[c(10, 1, 4, 6, 2, 5, 9)], labels = c("Forbs and graminoids", "Forbs, graminoids and bryophytes", "Graminoids and bryophytes", "Graminoids", "Bryophytes and forbs", "Forbs","Bryophytes")) +
  facet_wrap(~Treatment)

ggsave(SMplot, file = "~/Documents/seedclimComm/figures/smplot4.jpg", dpi = 300, width = 10, height = 4)


smCovPlot <- smVegAnom %>% 
  filter(between(date, ymd("2015-07-01"), ymd("2015-09-30")),
         Treatment %in% c("GF", "FB", "GB")) %>% 
  gather(key = response, value = value, graminoidCov, vegetationHeight, forbCov, mossHeight, mossCov) %>%
  mutate(response = factor(response, levels = c("graminoidCov", "forbCov", "mossCov", "vegetationHeight", "mossHeight"))) %>% 
  filter(value > 0,
         response %in% c("graminoidCov", "forbCov", "mossCov")) %>% 
  ggplot(aes(x = value, y = SMAnom, fill = response)) +
  stat_summary(geom = "point", fun.y = "mean", alpha = 0.9, shape = 21, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", size = 0.5) +
  scale_fill_manual(values = pal1[c(2,4,5)]) +
  #scale_fill_manual(values = c("black", "grey60")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Cover (%)",
       y = "moisture anomaly from bare ground")
ggsave(smCovPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/Fig4a.jpg", dpi = 300, width = 8, height = 5)


smHeightPlot <- smVegAnom %>% 
  filter(between(date, ymd("2015-07-01"), ymd("2015-09-30")),
         Treatment %in% c("GF", "FB", "GB")) %>% 
  gather(key = response, value = value, graminoidCov, vegetationHeight, forbCov, mossHeight, mossCov) %>%
  mutate(response = factor(response, levels = c("graminoidCov", "forbCov", "mossCov", "vegetationHeight", "mossHeight"))) %>% 
  filter(value > 0,
         response %in% c("vegetationHeight", "mossHeight")) %>% 
  ggplot(aes(x = value, y = SMAnom, fill = response)) +
  stat_summary(geom = "point", fun.y = "mean", alpha = 0.9, shape = 21, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", size = 0.5) +
  scale_fill_manual(values = pal1[c(2,4,5)]) +
  #scale_fill_manual(values = c("black", "grey60")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Height (mm)",
       y = "moisture anomaly from bare ground") +
  facet_grid(.~response, scales = "free_x")
ggsave(smHeightPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/Fig4b.jpg", dpi = 300, width = 8, height = 4)


#treatment
#### with TEMP ####
modSMT <- smVeg %>%
  filter(!Treatment == "XC") %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>%
  do({
    mod <- lmer(SM ~ scale(tempLevel)*Treatment +  (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

coefPlotsmT <- modSMT %>%
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!grepl("_|sunn", term)) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96),
         term = gsub("Treatment", "", term),
         term = gsub("^C", "Control", term),
         term = gsub(":C", ":Control", term),
         term = gsub("scale\\(|\\)", "", term)
         )  %>%
  mutate(term2 = if_else(grepl("Temp:", term), "PFG x t", "PFG")) %>%
  as.data.frame()

modSMT1 <- coefPlotsmT %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  scale_x_discrete(limits = c("Temp:Control", "Control", "Temp:F", "F", "Temp:B", "B", "Temp:G", "G", "Temp:FB", "FB", "Temp:GB", "GB", "Temp:GF", "GF", "Temp"), labels = c("Temp:Control" = "", "Control" = "C", "Temp:F" = "", "F" = "GB", "Temp:B" = "", "B" = "GF", "Temp:G" = "", "G" = "FB", "Temp:FB" = "", "FB" = "G", "Temp:GB" = "", "GB" = "F", "Temp:GF" = "", "GF" = "B", "Temp" = "Temperature")) +
  geom_vline(xintercept =  c(2.5,4.5,6.5,8.5,10.5,12.5, 14.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())


ggsave(modSMT1, file = "~/Documents/seedclimComm/figures/coefplotsmT2.jpg", dpi = 300, width = 6, height = 4)

modSMP <- SM201516 %>%
  filter(!Treatment == "XC") %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>%
  do({
    mod <- lmer(SM ~ scale(Precip)*Treatment +  (1|siteID/block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

coefPlotsmP <- modSMP %>%
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!grepl("_|sunn", term)) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96),
         term = gsub("Treatment", "", term),
         term = gsub("^C", "Control", term),
         term = gsub(":C", ":Control", term),
         term = gsub("scale\\(|\\)", "", term)
  )  %>%
  mutate(term2 = if_else(grepl("Precip:", term), "PFG x P", "PFG")) %>%
  as.data.frame()

modSMP1 <- coefPlotsmP %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  scale_x_discrete(limits = c("Precip:Control", "Control", "Precip:F", "F", "Precip:B", "B", "Precip:G", "G", "Precip:FB", "FB", "Precip:GB", "GB", "Precip:GF", "GF", "Precip"), labels = c("Precip:Control" = "", "Control" = "C", "Precip:F" = "", "F" = "GB", "Precip:B" = "", "B" = "GF", "Precip:G" = "", "G" = "FB", "Precip:FB" = "", "FB" = "G", "Precip:GB" = "", "GB" = "F", "Precip:GF" = "", "GF" = "B", "Precip" = "Precipitation")) +
  geom_vline(xintercept =  c(2.5,4.5,6.5,8.5,10.5,12.5, 14.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())


ggsave(modSMP1, file = "~/Documents/seedclimComm/figures/coefplotsmP2.jpg", dpi = 300, width = 6, height = 4)


#cover
### WITH TEMP ####
modSMTC <- smVeg %>%
  do({
    mod <- lmer(SM ~ scale(bryophyteCov)*scale(Temp) + scale(forbCov)*scale(Temp) + scale(graminoidCov)*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame

coefPlotsmTC <- modSMTC %>%
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!grepl("_|sunn", term)) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>%
  mutate(term2 = if_else(grepl("Temp:", term), "PFG x t", if_else(grepl(":Temp", term), "PFG x t", "PFG") )) %>%  
  as.data.frame()

coefPlotsmCT1 <- coefPlotsmTC %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2, linetype = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white")) +
  scale_shape_manual(legend.title.climate, values = c(21,24)) +
  scale_linetype_manual(legend.title.climate, values = c(1,1)) +
  scale_x_discrete(limits = c("bryophyteCov:Temp", "bryophyteCov", "Temp:graminoidCov", "graminoidCov", "Temp:forbCov", "forbCov", "Temp"), labels = c("bryophyteCov:Temp" = "", "bryophyteCov" = "bryophyte", "Temp:graminoidCov" = "", "graminoidCov" = "graminoid", "Temp:forbCov" = "", "forbCov" = "forb", "Temp" = "Temperature")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept =  c(2.5, 4.5, 6.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(coefPlotsmCT1, file = "~/Documents/seedclimComm/figures/coefplotsmCT2.jpg", dpi = 300, width = 6, height = 4)


##### with PRECIP ####
modSMCP <- smVeg %>%
  do({
    mod <- lmer(SM ~ scale(bryophyteCov)*scale(Precip) + scale(forbCov)*scale(Precip) + scale(graminoidCov)*scale(Precip) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame

coefPlotsmCP <- modSMCP %>%
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!grepl("_|sunn", term)) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>%
  mutate(term2 = if_else(grepl("Precip", term), "PFG x P", "PFG")) %>%  
  as.data.frame()

coefPlotsmCP1 <- coefPlotsmCP %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2, linetype = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white")) +
  scale_shape_manual(legend.title.climate, values = c(21,24)) +
  scale_linetype_manual(legend.title.climate, values = c(1,1)) +
  scale_x_discrete(limits = c("bryophyteCov:Precip", "bryophyteCov", "Precip:graminoidCov", "graminoidCov", "Precip:forbCov", "forbCov", "Precip"), labels = c("bryophyteCov:Precip" = "", "bryophyteCov" = "bryophyte", "Precip:graminoidCov" = "", "graminoidCov" = "graminoid", "Precip:forbCov" = "", "forbCov" = "forb", "Precip" = "Precipitation")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept =  c(2.5, 4.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")


ggsave(coefPlotsmCP1, file = "~/Documents/seedclimComm/figures/coefplotsmCP2.jpg", dpi = 300, width = 6, height = 4)



#height
##### with PRECIP #### 
modSMHP <- smVeg %>%
  do({
    mod <- lmer(SM ~ scale(vegetationHeight)*scale(Precip) + scale(mossHeight)*scale(Precip) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame

coefPlotsmHP <- modSMHP %>%
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!grepl("_|sunn", term)) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>%
  mutate(term2 = if_else(grepl("Precip", term), "PFG x P", "PFG")) %>% 
  filter(!term == "Precip") %>% 
  as.data.frame()

coefPlotsmHP1 <- coefPlotsmHP %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2, linetype = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white")) +
  scale_shape_manual(legend.title.climate, values = c(21,24)) +
  scale_linetype_manual(legend.title.climate, values = c(1,1)) +
  scale_x_discrete(limits = c("Precip:moss height", "moss height", "vegetationHeight:Precip", "vegetationHeight"), labels = c("vegetationHeight:Precip" = "", "vegetationHeight" = "vascular", "Precip:moss height" = "", "moss height" = "non-vascular")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept = c(2.5, 4.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(coefPlotsmHP1, file = "~/Documents/seedclimComm/figures/coefplotsmHP2.jpg", dpi = 300, width = 6, height = 4)


##### with TEMP #### 
modSMHT <- smVeg %>%
  do({
    mod <- lmer(SM ~ scale(vegetationHeight)*scale(Temp) + scale(mossHeight)*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame

coefPlotsmHT <- modSMHT %>%
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!grepl("_|sunn", term)) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>%
  mutate(term2 = if_else(grepl("Temp", term), "PFG x t", "PFG")) %>%  
  as.data.frame()

coefPlotsmHT1 <- coefPlotsmHT %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2, linetype = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white")) +
  scale_shape_manual(legend.title.climate, values = c(21,24)) +
  scale_linetype_manual(legend.title.climate, values = c(1,1)) +
  scale_x_discrete(limits = c("Temp:moss height", "moss height", "vegetationHeight:Temp", "vegetationHeight", "Temp"), labels = c("vegetationHeight:Temp" = "", "vegetationHeight" = "vascular", "Temp:moss height" = "", "moss height" = "non-vascular", "Temp" = "Temperature")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept = c(2.5, 4.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(coefPlotsmHT1, file = "~/Documents/seedclimComm/figures/coefplotsmHT2.jpg", dpi = 300, width = 6, height = 4)

smVegplotC <- smVeg %>% 
  gather(key = response, value = value, graminoidCov, vegetationHeight, forbCov, mossHeight, bryophyteCov) %>% 
  filter(value < 580, !Treatment == "FGB", value > 0, SMAnom < 25) %>%
  mutate(response = factor(response, levels = c("graminoidCov", "forbCov", "bryophyteCov", "vegetationHeight", "mossHeight"))) %>% 
  filter(response == "bryophyteCov") %>% 
  ggplot(aes(x = value, y = SMAnom, colour = vegCov, fill =  vegCov)) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_summary(geom = "point", fun.y = "mean", alpha = "0.5") +
  scale_colour_manual(values = cbPalette[c(1,4,5)]) +
  scale_fill_manual(values = cbPalette) +
  facet_wrap( ~ response, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  axis.dimLarge +
  labs(y = "soil moisture anomaly from bare ground")

#ggsave(smVegplot, file = "~/Documents/seedclimComm/figures/smvegPlot1a.jpg", dpi = 300, width = 9, height = 5)

smVegplotH <- smVeg %>% 
  gather(key = response, value = value, graminoidCov, vegetationHeight, forbCov, mossHeight, bryophyteCov) %>% 
  filter(value < 580, !Treatment == "FGB", value > 0, SMAnom < 25) %>%
  mutate(response = factor(response, levels = c("graminoidCov", "forbCov", "bryophyteCov", "vegetationHeight", "mossHeight"))) %>% 
  filter(response == "mossHeight") %>% 
  ggplot(aes(x = value, y = SMAnom, colour = vegCov, fill =  vegCov)) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_summary(geom = "point", fun.y = "mean", alpha = "0.5") +
  scale_colour_manual(values = cbPalette[c(1,4,5)]) +
  scale_fill_manual(values = cbPalette) +
  facet_wrap( ~ response, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  axis.dimLarge +
  labs(y = "soil moisture anomaly from bare ground")

legend <- get_legend(heightPlot1)

yplot <- plot_grid(coefPlotsmC, smVegplotC + theme(legend.position = "none"), coefPlotsmH, smVegplotH + theme(axis.title.y = element_blank(), legend.position = "none"), labels = c("A", "B", "C", "D"), rel_widths = c(0.83, 0.95), align = "h", axis = "lb")

coverPlot1gridSM <- plot_grid(yplot, legend, rel_widths = c(3, 0.3))
ggsave(coverPlot1gridSM, file = "~/Documents/seedclimComm/figures/responsePlotGrid1SM.jpg", dpi = 300, width = 9, height = 7)

######## PREDICTIONS ########
#### minimum temperature ####
modsmVeg <- smVeg %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  filter(Treatment %in% c("aFGB", "C", "FB", "GB", "GF"), SM > 0)

modTreatSM <- modsmVeg %>%
  glmer(SM ~ scale(Precip)*scale(Temp)*Treatment + (1|siteID/Block),family = Gamma(link = "inverse"), data = .)

P85 <- modsmVeg %>% mutate(Precip = Precip*1.17)
t85 <- modsmVeg %>% mutate(Temp = Temp + 3.9)
tP85 <- modsmVeg %>% mutate(Temp = Temp + 3.9,
                            Precip = Precip*1.17)

modsmVeg$modPreds <- predict(modTreatSM, re.form = ~1|siteID/Block)
modsmVeg$P85 <-  predict(modTreatSM, newdata = P85, re.form = ~1|siteID/Block)
modsmVeg$t85 <-  predict(modTreatSM, newdata = t85, re.form = ~1|siteID/Block)
modsmVeg$tP85 <-  predict(modTreatSM, newdata = tP85, re.form = ~1|siteID/Block)

pal8 <- wes_palette(8, name = "Darjeeling2", type = "continuous")

modsmVeg %>% gather(SM, modPreds, P85, t85, tP85, key = Model, value = value) %>% 
  ggplot(aes(x = P_level, y = value, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(.~Model) +
  scale_color_manual(values = pal8)

ggsave(minTempPredictions, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/minTempPredictions.jpg", dpi = 300, width = 13, height = 6)
