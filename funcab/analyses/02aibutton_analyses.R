#### ---- NOTES ---- ####
#use raw data, with bare ground as intercept
#use actual climate temperature and precipitations instead of level
# treatment*sunniness + treatment*temp

# how many sunny vs cloudy days are there?


#### --- prepare data --- ####
# load packages
library(lme4)
library(tidyverse)
library(broom)
library(lubridate)
library(wesanderson)

# source plotting code and soil temperature data
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/figures/plotting_dim.R")

load("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/ibuttons/maxmin.RData")

# filter for 1st July - 31st August for analyses, gather temperature calculationg, and recode FGB to be intercept
maxmin_a <- maxmin %>% 
  filter(between(date, left = dmy("01-07-2015"), right = dmy("20-10-2015"))) %>% 
  #gather(meanTemp, maxTemp, minTemp, magTemp, key = "temp", value = "value") %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB"))

Cover <- maxmin_a %>% 
  ungroup() %>% 
  mutate(Year = year(date),
         sTemp70 = scale(temp7010),
         sPrecip70 = scale(precip7010),
         sgraminoidCov = scale(graminoidCov),
         ssunniness = scale(sunniness),
         sforbCov = scale(forbCov),
         smossCov = scale(mossCov),
         svegetationHeight = scale(vegetationHeight),
         smossHeight = scale(mossHeight))

##### ------ Q.1 ------ ######
#Q1 – does removal of functional groups affect soil temperature?
#And if so, is one more important than another?
#treatment as predictor

maxmin_a %>% filter(temp == "maxTemp") %>% ggplot(aes(x = Treatment, y = value, colour = Treatment)) +
  geom_boxplot() +
  #stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  facet_grid(tempLevel~precipLevel)  

modTreat1 <- lmer(maxTemp ~ Treatment*sTemp70*sPrecip70*sunniness - Treatment:sTemp70:sPrecip70:sunniness + (1|siteID/blockID), REML = FALSE, data = Cover)

modTreat1 <- modTreat1 %>% 
  tidy() %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

modTreat1 <- modTreat1 %>% 
  filter(!grepl("^sd_", term),
         !term == "(Intercept)") %>% 
  mutate(term = gsub("sgraminoidCov", "graminoid", term),
         term = gsub("sforbCov", "forb", term),
         term = gsub("svegetationHeight", "vascular plant", term),
         term = gsub("smossHeight", "moss", term),
         term = gsub("smossCov", "moss", term),
         term = gsub("sPrecip70", "P", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("Treatment", "", term),
         term = gsub("sunniness", "UV", term),
         #term = gsub("(Intercept)", "bare ground", term),
         term = gsub(":", " x ", term)
  ) %>% 
  filter(!term %in% c("P x UV", "t x UV", "t x P x UV", "t x P")) %>% 
  mutate(term2 = if_else(grepl("x UV", term) & nchar(term) < 8, "PFG x UV",
                    if_else(grepl("x t", term) & nchar(term) < 7, "PFG x t",
                         if_else(grepl("x P", term) & nchar(term) < 7, "PFG x P",
                         if_else(grepl("x t x P", term) & nchar(term) > 7, "PFG x t x P", 
                         if_else(grepl("x t", term) & nchar(term) > 7, "PFG x UV x t",
                         if_else(grepl("x P", term) & nchar(term) > 7, "PFG x UV x P", 
                                 if_else(nchar(term) < 3 & !term %in% c("t", "UV", "P"), "PFG", term
                                         ))))))))

plotmodTreat1 <- modTreat1 %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, colour = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #scale_shape_manual(legend.title.climate, values = c(21,24, 23)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_colour_manual(legend.title.climate, values = cbPalette) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("UV", "t", "P", 
                              "C", "C x t", "C x P", "C x UV", "C x t x UV", "C x P x UV", "C x t x P", 
                              "F", "F x t", "F x P", "F x UV", "F x t x UV", "F x P x UV", "F x t x P",
                              "G", "G x t", "G x P", "G x UV", "G x t x UV", "G x P x UV", "G x t x P",
                              "B", "B x t", "B x P", "B x UV", "B x t x UV", "B x P x UV", "B x t x P",
                              "GF", "GF x t", "GF x P", "GF x UV", "GF x t x UV", "GF x P x UV", "GF x t x P",
                              "GB", "GB x t", "GB x P", "GB x UV", "GB x t x UV", "GB x P x UV", "GB x t x P",
                              "FB", "FB x t", "FB x P", "FB x UV", "FB x t x UV", "FB x P x UV", "FB x t x P")) +
  geom_vline(xintercept =  c(3.5, 10.5, 17.5, 24.5, 31.5, 38.5, 45.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients", x = "Functional group")

ggsave(plotmodTreat1, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig1.jpg", dpi = 300, width = 5.5, height = 8)



##### ------ Q.2 ------ ######
#Q2 – if it seems that none is particularly more important than the others, is it purely a percent cover effect? 
#Ie, removing x% of any functional group has the same effect, regardless of the functional group?

### ---- cover ---- ###
modGramCov <- Cover %>%
  filter(Treatment %in% c("B","F","C","FB")) %>%
  lmer(maxTemp ~ sgraminoidCov*sTemp70*sPrecip70*sunniness + (1|siteID/blockID), data = .)
plot(modGramCov)
require('lattice'); qqmath(modGramCov)
modGramCov <- tidy(modGramCov)

modForbCov <- Cover %>% 
  filter(Treatment %in% c("B","G","C","GB")) %>%
  lmer(maxTemp ~ sforbCov*sTemp70*sPrecip70*sunniness + (1|siteID/blockID), data = .)
plot(modForbCov)
qqmath(modForbCov)
modForbCov <- tidy(modForbCov)

modMossCov <- Cover %>% 
  filter(Treatment %in% c("G","F","C","GF")) %>%
  lmer(maxTemp ~ smossCov*sTemp70*sPrecip70*sunniness + (1|siteID/blockID), data = .)
plot(modMossCov)
qqmath(modMossCov)
modMossCov <- tidy(modMossCov)

### ---- height ---- ###
modVegH <- Cover %>% 
  filter(Treatment %in% c("B","F","G","C","FB","GB")) %>%
  lmer(maxTemp ~ svegetationHeight*sTemp70*sPrecip70*sunniness + (1|siteID/blockID), data = .)
plot(modVegH) 
qqmath(modVegH)
modVegH <- tidy(modVegH)

modMossH <- Cover %>% 
  filter(Treatment %in% c("GF","F","G","C")) %>%
  lmer(maxTemp ~ smossHeight*sTemp70*sPrecip70*sunniness + (1|siteID/blockID), data = .)
plot(modMossH)
qqmath(modMossH)
modMossH <- tidy(modMossH)

all <- bind_rows("gramCov" = modGramCov, "forbCov" = modForbCov, "mossCov" = modMossCov, "mossH" = modMossH, "vegH" = modVegH, .id = "model") %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))
  

allMod <- all %>% 
  filter(!grepl("^sd_", term),
         !term == "(Intercept)") %>% 
  mutate(term = gsub("sgraminoidCov", "graminoid", term),
         term = gsub("sforbCov", "forb", term),
         term = gsub("svegetationHeight", "vascular plant", term),
         term = gsub("smossHeight", "moss", term),
         term = gsub("smossCov", "moss", term),
         term = gsub("sPrecip70", "P", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("sunniness", "UV", term),
         #term = gsub("(Intercept)", "bare ground", term),
         term = gsub(":", " x ", term)
         )

allMod %>% 
  filter(model %in%c("gramCov", "forbCov", "mossCov")) %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  coord_flip() +
  facet_wrap(~ model, scales = "free_y") +
  theme(axis.title.y = element_blank())

allMod %>% 
  filter(model %in%c("vegH", "mossH")) %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  coord_flip() +
  facet_wrap(~ model, scales = "free_y") +
  theme(axis.title.y = element_blank())


ggsave(vegCompmod1plotCALL, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3A.jpg", dpi = 300, width = 6, height = 4)

allMod %>% 
  select(-(group:upper)) %>% 
  filter(!is.na(term)) %>% 
  mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), statistic = round(statistic, 3)) %>% 
  gather(estimate, std.error, statistic, key = output, value = value) %>% 
  unite(cols, model, output) %>% 
  spread(key = cols, value = value) %>% 
  write_csv(., path = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/data/mod1-6OUT.csv")


##### ------ Q.3 ------ ######
# Q3. do FGs regulate soil freezing? Ie, is the persistent of vegetation matter on the soil surface at all important in preventing dangerous freezing events in the autumn and spring?

FD  <- FD %>% 
  ungroup() %>% 
  mutate(Year = year(date),
         sTemp70 = scale(temp7010),
         sPrecip70 = scale(precip7010),
         sgraminoidCov = scale(graminoidCov),
         ssunniness = scale(sunniness),
         sforbCov = scale(forbCov),
         smossCov = scale(mossCov),
         svegetationHeight = scale(vegetationHeight),
         smossHeight = scale(mossHeight))

modFD <- FD %>% 
  lmer(sum ~ Treatment*sTemp70*sPrecip70 + (1|siteID/blockID), REML = FALSE, data = .)

modFD <- tidy(modFD)

modFD <- FD %>%
  filter(Treatment %in% c("B","F","C","FB")) %>%
  do({
    mod <- lmer(sum ~ scale(graminoidCov)*scale(temp7010)*scale(precip7010) + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)
  })

allMod <- modFD %>% 
  filter(!grepl("^sd_", term),
         !term == "(Intercept)") %>% 
  mutate(term = gsub("sgraminoidCov", "graminoid", term),
         term = gsub("sforbCov", "forb", term),
         term = gsub("svegetationHeight", "vascular plant", term),
         term = gsub("smossHeight", "moss", term),
         term = gsub("smossCov", "moss", term),
         term = gsub("sPrecip70", "P", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("sunniness", "UV", term),
         #term = gsub("(Intercept)", "bare ground", term),
         term = gsub(":", " x ", term)
  )

modFD %>% 
  select(-(group:upper)) %>% 
  filter(!is.na(term)) %>% 
  mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), statistic = round(statistic, 3)) %>% 
  gather(estimate, std.error, statistic, key = output, value = value) %>% 
  unite(cols, model, output) %>% 
  spread(key = cols, value = value) %>% 
  write_csv(., path = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/data/mod1-6OUT.csv")

P85 <- FD %>% mutate(Precip = Precip*1.17)
t85 <- FD %>% mutate(Temp = Temp + 3.9)
tP85 <- FD %>% mutate(Temp = Temp + 3.9,
                         Precip = Precip*1.17)

FD$modPreds <- predict(modFD, re.form = ~1|blockID)
FD$P85 <-  predict(modFD, newdata = P85, re.form = ~1|blockID)
FD$t85 <-  predict(modFD, newdata = t85, re.form = ~1|blockID)
FD$tP85 <-  predict(modFD, newdata = tP85, re.form = ~1|blockID)

FD %>% gather(sum, modPreds, P85, t85, tP85, key = Model, value = value) %>% 
  ggplot(aes(x = Temperature_level, y = value, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(.~Model) +
  scale_color_manual(values = pal1) +
  geom_hline(yintercept = 0, colour = "grey60")

#ggsave(maxTempPredictions, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/maxTempPredictions.jpg", dpi = 300, width = 13, height = 6)



#########################################
######## TEMPERTAURE ANOMALIES ##########

# sum of degrees above 0 in August, September and October
DDsum <- vegComp %>% 
  distinct(turfID, date, TOD, Value, .keep_all = TRUE) %>% 
  filter(between(date, ymd("2015-08-01"), ymd("2015-12-31"))) %>%
  mutate(month = month(date)) %>% 
  filter(Value > 5) %>% 
  group_by(siteID, month, blockID, Treatment, Temperature_level) %>% 
  summarise(tempSum = sum(Value))

frostDayMod <- vegComp %>%
  distinct(turfID, date, TOD, Value, .keep_all = TRUE) %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-01"))) %>%
  #mutate(Treatment = factor(Treatment, levels = c("C", "FB", "GB", "GF", "FGB"))) %>% 
  group_by(date, siteID, turfID, Treatment, Block, sunniness, Temperature_level, Precipitation_level, litter, graminoidCov, forbCov, bryophyteCov, graminoid, forb, mossHeight, vegetationHeight) %>% 
  summarise(maxTemp = max(Value),
            minTemp = min(Value)) %>% 
  mutate(x = minTemp < 0) %>%
  distinct(turfID, date, .keep_all = TRUE) %>% 
  arrange(date) %>% 
  group_by(Treatment, siteID, turfID) %>% 
  mutate(sum = cumsum(x), n = n()) %>%
  mutate(Temp = recode(siteID, Ulvhaugen=6.17, Lavisdalen =6.45, Gudmedalen =5.87, Skjellingahaugen =6.58, Alrust =9.14, Hogsete =9.17, Rambera =8.77, Veskre =8.67, Fauske =10.3, Vikesland =10.55, Arhelleren =10.60, Ovstedal=10.78),
         Precip= recode(siteID, Ulvhaugen =596, Lavisdalen =1321, Gudmedalen =1925, Skjellingahaugen =2725, Alrust =789, Hogsete =1356, Rambera =1848, Veskre =3029, Fauske =600, Vikesland =1161, Arhelleren =2044, Ovstedal=2923)) %>% 
  filter(!turfID == "Lav3GF") %>% 
  filter(date == ymd("2016-05-01"))

frostDayMod <- frostDayMod %>% 
  group_by(turfID, Treatment, Block, siteID) %>%
  left_join(frostDayMod %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBsum = sum, date, siteID, Block)) %>%
  mutate(sumAnom = sum - FGBsum) %>% 
  ungroup() #%>% 
  #filter(date == "2016-05-30")

x <- frostDayMod %>% 
  filter(!Treatment == "temp200cm", siteID == "Lavisdalen") %>% #work in progress
  ungroup() %>% 
  ggplot(aes(x = date, y = sum, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line", size = 0.8) +
  #geom_point(size = 0.8) +
  scale_x_discrete(limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C"), labels = c("bare","B", "F", "G", "FB", "GB", "GF", "FGB")) +
  scale_colour_manual("Vegetation", values = c("black", cbPalette[c(7,5,6,2,4,10,1)]), limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C")) +
  scale_fill_manual("Vegetation", values = c("black", cbPalette[c(7,5,6,2,4,10,1)]), limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  axis.dimLarge +
  ylab("Cumulative frost\ndays") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks=element_blank(),
        axis.line.x = element_blank(),
        axis.title=element_text(size=15))

frostDayMod1T <- frostDayMod %>% 
  ungroup() %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  do({
    mod <- lmer(sum ~ Treatment*scale(Precip) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

frostDayMod1Tt <- frostDayMod1T %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(term2 = if_else(grepl("Precip:", term), "PFG x t",
                         if_else(grepl(":Precip", term), "PFG x t", "PFG")))

frostDayMod1H <- frostDayMod %>% 
  ungroup() %>% 
  do({
    mod <- lmer(maxTemp ~ vegetationHeight*scale(Precip) + mossHeight*scale(Precip) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


frostDayMod1Ht <- frostDayMod1H %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(term2 = if_else(grepl("Precip:", term), "PFG x t",
                         if_else(grepl(":Precip", term), "PFG x t", "PFG")))
  
frostDayMod1C <- frostDayMod %>% 
  ungroup() %>% 
  do({
    mod <- lmer(maxTemp ~ bryophyteCov*scale(Temp) + forbCov*scale(Temp) + graminoidCov*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

frostDayMod1Ct <- frostDayMod1C %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(term2 = if_else(grepl("Temp:", term), "PFG x t",
                         if_else(grepl(":Temp", term), "PFG x t", "PFG")))



frostDayModplot <- frostDayMod1C %>% 
  filter(!term %in% c("(Intercept)", "scale(Temp)"), !(grepl("sd", term))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_vline(xintercept =  c(1.5,2.5,3.5,4.5,5.5,6.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge

ggsave(frostDayModplot, filename = "~/Documents/seedclimComm/figures/frostDayMod2Cover.jpg", dpi = 300, width = 6, height = 4)


