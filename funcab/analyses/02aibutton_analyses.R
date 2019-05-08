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
library(broom.mixed)
library(lubridate)
library(wesanderson)
library(DHARMa)
library(glmmTMB)

# source plotting code and soil temperature data
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/plotting_dim.R")


# filter for 1st July - 31st August for analyses, gather temperature calculationg, and recode FGB to be intercept
maxmin <- maxmin %>% 
  filter(between(date, left = dmy("01-07-2015"), right = dmy("15-09-2015"))) %>%
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB"))

# standardise variables
Cover <- maxmin %>% 
  ungroup() %>% 
  mutate(Year = year(date),
         sTemp70 = scale(temp7010),
         sPrecip70 = scale(precip7010),
         ssunniness = scale(sunniness),
         sforbBiomass = scale(forbBiomass),
         smossBiomass = scale(mossBiomass),
         sgraminoidBiomass = scale(graminoidBiomass),
         sforbCov = scale(forbCov),
         smossCov = scale(mossCov),
         sgraminoidCov = scale(graminoidCov),
         svegetationHeight = scale(vegetationHeight),
         smossHeight = scale(mossHeight))

# moving window
sumAT <- maxmin %>% 
  filter(Treatment == "aFGB", weather == "sunny") %>% 
  summarise(sumAT = mean(maxTemp),
            LowersumAT = sumAT - 2.5,
            UppersumAT = sumAT + 2.5)

maxmin %>% 
  group_by(date, turfID) %>% 
  mutate(window = if_else(between(maxTemp, sumAT$LowersumAT, sumAT$UppersumAT), 1, 0)) %>% 
  filter(weather == "sunny", Treatment == "aFGB") %>% 
  ggplot(aes(x = date, y = maxTemp)) + 
  geom_point(alpha = 0.5, colour = "grey", size = 3, shape = 21, aes(fill = factor(window))) + 
  facet_grid(.~tempLevel) +
  geom_hline(yintercept = sumAT$sumAT) +
  geom_ribbon(ymin = sumAT$LowersumAT, ymax = sumAT$UppersumAT, alpha = 0.3, fill = "tan1") +
  scale_fill_manual(values = c("black", "darkgoldenrod"))

# maxmin_a = window
maxmin_a <- maxmin %>%
  group_by(date, turfID) %>% 
  mutate(window = if_else(between(maxTemp, sumAT$LowersumAT, sumAT$UppersumAT), 1, 0)) %>% 
  filter(window == 1, weather == "sunny")



# supplementary figures
g <- Cover %>% filter(Treatment == "FB") %>% 
  ggplot(aes(x = graminoidCov)) + geom_density(alpha = 0.3, fill = "goldenrod") + 
  geom_rug() + 
  geom_vline(xintercept = 10, linetype = "dashed") + 
  facet_grid(.~tempLevel) +
  xlim(0,100) +
  labs(x = "graminoid cover (%)")
f <- Cover %>% filter(Treatment == "GB") %>% 
  ggplot(aes(x = forbCov)) + geom_density(alpha = 0.3, fill = "goldenrod") + 
  geom_rug() + 
  geom_vline(xintercept = 10, linetype = "dashed") + 
  facet_grid(.~tempLevel) +
  xlim(0,100) +
  labs(x = "forb cover (%)")
m <- Cover %>% filter(Treatment == "GF") %>% 
  ggplot(aes(x = mossCov)) + geom_density(alpha = 0.3, fill = "goldenrod") + 
  geom_rug() + 
  geom_vline(xintercept = 10, linetype = "dashed") + 
  facet_grid(.~tempLevel) +
  xlim(0,100) +
  labs(x = "bryophyte cover (%)")

fgmDens <- plot_grid(g + theme(axis.title.y = element_blank()),f,m + theme(axis.title.y = element_blank()), ncol = 1,align = "hv")
ggsave(fgmDens, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig9.jpg", dpi = 300, height = 7, width = 7)


##### ------ Q.1 ------ ######
# Q1 – does removal of functional groups affect soil temperature? Is one more important than another?
# treatment and UV as predictors

Cover_a <- Cover %>% 
  filter((graminoidCov > 10 & Treatment == "FB") | (mossCov > 10 & Treatment == "GF") | (forbCov > 10 & Treatment == "GB") | Treatment == "aFGB" | (graminoidCov > 10 & mossCov > 10 & forbCov > 10 & Treatment == "C")) %>% 
  filter(weather %in% c("sunny", "cloudy"))

modTreat1 <- Cover_a %>% 
  group_by(weather) %>% 
  do({
  mod <- lmer(maxTemp ~ Treatment*sTemp70*sPrecip70 + (1|siteID/blockID), REML = FALSE, data = .)
  tidy(mod)
  }) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

modTreat1 <- modTreat1 %>% 
  filter(!grepl("^sd_", term),
         !term == "(Intercept)") %>% 
  mutate(term = gsub("GB", "F", term),
         term = gsub("GF", "B", term),
         term = gsub("FB", "G", term),
         term = gsub("smossHeight", "moss", term),
         term = gsub("smossCov", "moss", term),
         term = gsub("sPrecip70", "P", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("Treatment", "", term),
         term = gsub(":", " x ", term)
  ) %>% 
  mutate(term2 = if_else(grepl("x t", term) & nchar(term) < 7, "PFG x t",
                         if_else(grepl("x P", term) & nchar(term) < 7, "PFG x P",
                         if_else(grepl("x t x P", term) & nchar(term) > 7, "PFG x t x P", "Main effects", term
                                         )))) %>% 
  mutate(term3 = case_when(
    grepl("G", term) ~ "Graminoids",
    grepl("F", term) ~ "Forbs",
    grepl("B", term) ~ "Bryophytes",
    grepl("C", term) ~ "Intact vegetation",
    term %in% c("t", "P") ~ "Climate"
    ))

plotmodTreat1 <- modTreat1 %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, shape = term3)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept =  c(2.5, 6.5, 10.5), colour = pal1[c(5,2,4,5,2,4)], size = 27, alpha = 0.2) +
  geom_vline(xintercept = 13.5, colour = "grey", size = 13.5, alpha = 0.2) +
  geom_point(position = position_dodge(width = 0.1), size = 3, fill = "grey") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) + 
  scale_shape_manual("", values = c(21, 22, 23, 24, 25,1), limits = c("Graminoids", "Forbs", "Bryophytes", "Intact vegetation", "Climate")) + 
  scale_x_discrete(limits = c("C x t x P", "B x t x P", "F x t x P", "G x t x P",
                              "C x P", "B x P", "F x P", "G x P",
                              "C x t", "B x t", "F x t", "G x t",
                              "t", "P", "C", "B", "F", "G")) +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  facet_grid(.~weather) +
  theme_classic() +
  axis.dimLarge +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

ggsave(plotmodTreat1, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3a.jpg", dpi = 300, width = 10, height = 5)

# predictions figures
modTreat1pred <- Cover_a %>% 
  group_by(weather) %>% 
  do({
    mod <- lmer(maxTemp ~ Treatment*sTemp70*sPrecip70 + (1|siteID/blockID), REML = FALSE, data = .)
    augment(mod, data = .)
  })

modTreat1predPlot <- modTreat1pred %>%
  left_join(modTreat1pred %>% filter(Treatment == "aFGB") %>% ungroup() %>% select(FGBmaxTemp = .fitted, date, siteID, blockID)) %>%
  mutate(FittedmaxAnom = .fitted - FGBmaxTemp) %>% 
  filter(!Treatment == "aFGB") %>% 
  mutate(term3 = case_when(
    grepl("FB", Treatment) ~ "Graminoids",
    grepl("GB", Treatment) ~ "Forbs",
    grepl("GF", Treatment) ~ "Bryophytes",
    grepl("C", Treatment) ~ "Intact vegetation"
  )) %>% 
  ggplot(aes(x = tempLevel, y = FittedmaxAnom, linetype = weather, shape = term3)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", fill = "grey", size = 0.75) +
  geom_hline(yintercept = 0, colour = "grey70", lty = "dashed") +
  scale_shape_manual("", values = c(21,22,23,24), limits = c("Graminoids", "Forbs", "Bryophytes", "Intact vegetation")) + 
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) +
  scale_linetype_discrete("", limits = c("sunny", "cloudy"), guide = FALSE) +
  labs(x = "Mean summer temperature (ºC)", y = "Δ soil temperature\n(ºC; treatment - bare soil)") +
  axis.dimLarge +
  theme(legend.position = "none")

#ggsave(modTreat1predPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig4a.jpg", dpi = 300, width = 5, height = 4)

leg <- get_legend(plotmodTreat1)
upper <- plot_grid(plotmodTreat1 + theme(legend.position = "none"), modTreat1predPlot, nrow = 1, rel_widths = c(1,0.8), labels = c("A", "B"), align = "h", axis = "tblr")
tot <- plot_grid(upper, leg, ncol = 1, rel_heights = c(1,0.1))

ggsave(tot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3.jpg", dpi = 300, width = 10, height = 5)

# model output
STmod <- modTreat1 %>% 
  filter(!is.na(term)) %>% 
  mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), statistic = round(statistic, 3))
write_csv(STmod, path = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/data/soilTempMod.csv")

##### ------ Q.3 ------ ######
# Q3. do FGs regulate soil freezing? Ie, is the persistent of vegetation matter on the soil surface at all important in preventing dangerous freezing events in the autumn and spring?
FDanalysis <- FD %>% filter(date == dmy("30-05-2016"))

FDanalysis  <- FDanalysis %>% 
  ungroup() %>% 
  left_join(weather) %>% 
  mutate(Year = year(date),
         sTemp70 = scale(temp7010),
         sPrecip70 = scale(precip7010),
         sgraminoidCov = scale(graminoidCov),
         ssunniness = scale(sunniness),
         sforbCov = scale(forbCov),
         smossCov = scale(mossCov),
         svegetationHeight = scale(vegetationHeight),
         smossHeight = scale(mossHeight))

# is a zero-inflated model the best model? Unsure...

FDanalysis <- FDanalysis %>% 
  filter(Treatment %in% c("GF", "GB", "FB", "FGB", "C")) %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  filter((graminoidCov > 10 & Treatment == "FB") | (mossCov > 10 & Treatment == "GF") | (forbCov > 10 & Treatment == "GB") | Treatment == "aFGB" | (graminoidCov > 10 & mossCov > 10 & forbCov > 10 & Treatment == "C"))

modFDtreat <- glmmTMB(sum ~ Treatment*sTemp70*sPrecip70 + (1|siteID/blockID), zi = ~ 1, family = "poisson", data = FDanalysis)

simulationOutput <- simulateResiduals(modFDtreat, n = 250)
plot(simulationOutput)
testZeroInflation(simulationOutput)
summary(modFDtreat)

modFDtreat <- tidy(modFDtreat) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

allFDmod <- modFDtreat %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("sPrecip70", "P", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("TreatmentFB", "G", term),
         term = gsub("TreatmentGB", "F", term),
         term = gsub("TreatmentGF", "B", term),
         term = gsub("TreatmentC", "C", term),
         term = gsub(":", " x ", term)
  )%>% 
  mutate(term2 = if_else(grepl("x t", term) & nchar(term) < 7, "PFG x t",
                         if_else(grepl("x P", term) & nchar(term) < 7, "PFG x P",
                                 if_else(grepl("x t x P", term) & nchar(term) > 7, "PFG x t x P", "Main effects", term
                                 )))) %>% 
  mutate(term3 = case_when(
    grepl("G", term) ~ "Graminoids",
    grepl("F", term) ~ "Forbs",
    grepl("B", term) ~ "Bryophytes",
    grepl("C", term) ~ "Intact vegetation",
    term %in% c("t", "P") ~ "Climate"
  ))

allFDmodplot <- allFDmod %>%
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
  ylim(-2.8, 1.8) +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme_classic() +
  axis.dimLarge +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

#ggsave(allFDmodplot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3b.jpg", dpi = 300, width = 5, height = 4.7)

# predictions
modFDtreatPred <- FDanalysis %>% 
  do({
    mod <- glmmTMB(sum ~ Treatment*sTemp70*sPrecip70 + (1|siteID/blockID), zi = ~ 1, family = "poisson", data = .)
    augment(mod, data = .)
  })

allModPlotPred <- modFDtreatPred %>%
  left_join(modFDtreatPred %>% filter(Treatment == "aFGB") %>% ungroup() %>% select(FGBmaxTemp = .fitted, date, siteID, blockID)) %>%
  mutate(FittedFDAnom = .fitted - FGBmaxTemp) %>% 
  filter(!Treatment == "aFGB") %>% 
  mutate(term3 = case_when(
    grepl("FB", Treatment) ~ "Graminoids",
    grepl("GB", Treatment) ~ "Forbs",
    grepl("GF", Treatment) ~ "Bryophytes",
    grepl("C", Treatment) ~ "Intact vegetation"
  )) %>% 
  ggplot(aes(x = precipLevel, y = FittedFDAnom, shape = term3)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line", position = position_dodge(width = 100)) +
  stat_summary(fun.data = "mean_cl_boot", fill = "grey", size = 0.75, position = position_dodge(width = 100)) +
  geom_hline(yintercept = 0, colour = "grey70", lty = "dashed") +
  scale_shape_manual("", values = c(21,22,23,24), limits = c("Graminoids", "Forbs", "Bryophytes", "Intact vegetation")) + 
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) +
  scale_linetype_discrete("", limits = c("sunny", "cloudy"), guide = FALSE) +
  labs(x = "Total annual precipitation (mm)", y = "Δ frost days (treatment - bare soil)") +
  axis.dimLarge +
  theme(legend.position = "none")

leg <- get_legend(allFDmodplot)
upper <- plot_grid(allFDmodplot + theme(legend.position = "none"), allModPlotPred, nrow = 1, labels = c("A", "B"), align = "h", axis = "tblr")
tot <- plot_grid(upper, leg, ncol = 1, rel_heights = c(1,0.1))

ggsave(tot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig4.jpg", dpi = 300, width = 9, height = 5.5)


# model output
allFDmod <- allFDmod %>% 
  filter(!is.na(term)) %>% 
  mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), statistic = round(statistic, 3), p.value = round(p.value, 4))
write_csv(allFDmod, path = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/data/frostDayMod.csv")


##### ------ Q.4 ------ #####
#Q2 – if no FG is particularly more important than the others, is it purely a percent cover effect? 
#Ie, removing x% of any functional group has the same effect, regardless of the functional group?

Cover_b <- maxminAnom %>% 
  filter(between(date, left = dmy("01-07-2015"), right = dmy("15-09-2015"))) %>%
  mutate(Year = year(date)) %>% 
  mutate(Year = year(date),
         sTemp70 = scale(temp7010),
         sPrecip70 = scale(precip7010),
         ssunniness = scale(sunniness),
         sforbBiomass = scale(forbBiomass),
         smossBiomass = scale(mossBiomass),
         sgraminoidBiomass = scale(graminoidBiomass),
         sforbCov = scale(forbCov),
         smossCov = scale(mossCov),
         sgraminoidCov = scale(graminoidCov),
         svegetationHeight = scale(vegetationHeight),
         smossHeight = scale(mossHeight))

Cover_b <- Cover %>% 
  filter((graminoidCov > 10 & Treatment == "FB") | (mossCov > 10 & Treatment == "GF") | (forbCov > 10 & Treatment == "GB") | Treatment == "aFGB" | (graminoidCov > 10 & mossCov > 10 & forbCov > 10 & Treatment == "C")) %>% 
  filter(weather == "sunny")

#### with sunniness as predictor 
### cover
#graminoids
CoverG <- Cover_b %>% 
  filter(Treatment%in% c("FB"), weather == "sunny") %>% 
  select(maxAnom, sgraminoidCov, sTemp70, sPrecip70, siteID, blockID, Treatment, weather, graminoidCov) %>% 
  na.omit()

modGramCov <- lmer(maxAnom ~ sgraminoidCov*sTemp70*sPrecip70 + (1|siteID/blockID), REML = TRUE, data = CoverG)

plot(modGramCov)
require('lattice'); qqmath(modGramCov)
modGCpred <- augment(modGramCov) %>% 
  left_join(weather) 
modGCpred <- bind_cols(modGCpred, CoverG %>% select(Treatment, weather, graminoidCov))
modGramCov <- tidy(modGramCov)

# temperature
modGCpredPlot <- modGCpred %>% 
  ggplot(aes(x = sgraminoidCov, y = .fitted)) + 
  geom_point(alpha = 0.3, colour = "grey") + 
  geom_abline(intercept = -0.144, slope = -0.102) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) +
  labs(x = "scaled graminoid cover") +
  theme(axis.title.y = element_blank())

ggsave(file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3d.jpg", dpi = 300, width = 6, height = 4.5)

# moss
CoverM <- Cover_b %>% 
  filter(Treatment == "GF", weather == "sunny") %>% 
  select(maxAnom, smossCov, sTemp70, sPrecip70, ssunniness, siteID, blockID,Treatment, weather, mossCov) %>% 
  na.omit()

modMossCov <- lmer(maxAnom ~ smossCov*sTemp70*sPrecip70 + (1|siteID/blockID), REML = TRUE, data = CoverM)

plot(modMossCov)
require('lattice'); qqmath(modMossCov)
modMCpred <- augment(modMossCov) %>% 
  left_join(weather) 
modMCpred <- bind_cols(modMCpred, CoverM %>% select(Treatment, weather, mossCov))
modMossCov <- tidy(modMossCov)

# temperature
modMCpredPlot <- modMCpred %>% 
  ggplot(aes(x = smossCov, y = .fitted)) + 
  geom_point(alpha = 0.3, colour = "grey") + 
  geom_abline(intercept = -0.483, slope = -0.143) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) +
labs(x = "scaled moss cover", y = "fitted temperature anomaly from bare ground")

ggsave(file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3c.jpg", dpi = 300, width = 6, height = 4.5)


### height
# forb height
CoverVH <- Cover_b %>% 
  filter(Treatment %in% c("GB", "FB"), weather == "sunny") %>% 
  select(maxAnom, svegetationHeight, sTemp70, sPrecip70, ssunniness, siteID, blockID, Treatment, weather) %>% 
  na.omit()

modVH <- lmer(maxAnom ~ svegetationHeight*sTemp70*sPrecip70 + (1|siteID/blockID), REML = TRUE, data = CoverVH)

plot(modVH)
require('lattice'); qqmath(modVH)
modVHpred <- augment(modVH) %>% 
  left_join(weather) 
modVHpred <- bind_cols(modVHpred, CoverVH %>% select(Treatment, weather))
modVH <- tidy(modVH)

# temperature
modVHpred %>%
  ggplot(aes(x = svegetationHeight, y = .fitted, colour = factor(tempLevel))) + 
  geom_point(alpha = 0.3, colour = "grey") + 
  geom_abline(intercept = -1.12, slope = 0.116) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)])


ggsave(fhPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fhPlot.jpg", dpi = 300, width = 8, height = 4.5)


# moss height
CoverMH <- Cover_b %>% 
  filter(Treatment == "GF", weather == "sunny") %>% 
  select(maxAnom, smossHeight, sTemp70, sPrecip70, ssunniness, siteID, blockID, Treatment, weather, mossHeight) %>% 
  na.omit()

modMossH <- lmer(maxAnom ~ smossHeight*sTemp70*sPrecip70 + (1|siteID/blockID), REML = TRUE, data = CoverMH)

plot(modMossH)
require('lattice'); qqmath(modMossH)
modMHpred <- augment(modMossH) %>% 
  left_join(weather) 
modMHpred <- bind_cols(modMHpred, CoverMH %>% select(Treatment, weather, mossHeight))
modMossH <- tidy(modMossH)

# temperature
modMHpred %>% filter(weather %in% c("sunny", "cloudy")) %>% 
  ggplot(aes(x = smossHeight, y = .fitted, colour = factor(tempLevel))) + 
  geom_point(alpha = 0.3, colour = "grey") + 
  geom_smooth(method = "lm") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) + 
  facet_grid(. ~ weather)

# precipitation
modMHpred %>% filter(weather %in% c("sunny", "cloudy")) %>% 
  ggplot(aes(x = mossHeight, y = .fitted, colour = factor(precipLevel))) + 
  geom_point(alpha = 0.3, colour = "grey") + 
  geom_smooth(method = "lm") +
  scale_colour_manual("", values = pal1[c(2,4,5,1)]) + 
  facet_grid(. ~ weather)


all <- bind_rows("gramCov" = modGramCov, "forbCov" = modForbCov, "mossCov" = modMossCov, "mossH" = modMossH, "gramH" = modGH, "forbH" = modFH, .id = "model") %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

allMod <- all %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("sgraminoidCov", "graminoid", term),
         term = gsub("sforbCov", "forb", term),
         term = gsub("sforbHeight", "forb", term),
         term = gsub("sgraminoidHeight", "graminoid", term),
         term = gsub("smossHeight", "moss", term),
         term = gsub("smossCov", "moss", term),
         term = gsub("sPrecip70", "Precip", term),
         term = gsub("sTemp70", "t", term),
         term = gsub("ssunniness", "UV", term),
         term = gsub(":", " x ", term)
         )

modCovPlot <- allMod %>% 
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

ggsave(modCovPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig2a.jpg", dpi = 300, width = 10, height = 4)


modHeiPlot <- allMod %>% 
  filter(model %in%c("forbH", "gramH", "mossH"), !term == "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  coord_flip() +
  facet_wrap(~ model, scales = "free_y") +
  theme(axis.title.y = element_blank())

ggsave(modCovPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig2a.jpg", dpi = 300, width = 10, height = 4)


allMod <- allMod %>%
  filter(!is.na(term)) %>% 
  mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), statistic = round(statistic, 3))

write_csv(allMod, path = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/data/soilTempMod.csv")


#### predictions ####

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