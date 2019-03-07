library(lme4)
library(tidyverse)
library(broom)
library(lubridate)
library(wesanderson)

source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/plotting_dim.R")

load("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/ibuttons/maxmin.RData")

# filter for 1st July - 31st August for analyses
maxminANALYSIS <- maxmin %>% 
  filter(between(date, left = dmy("01-07-2015"), right = dmy("31-08-2015")))

#use raw data, with bare ground as intercept
#use actual climate temperature and precipitations instead of level
# treatment*sunniness + treatment*temp

# how many sunny vs cloudy days are there?

#### FUNCTIONS ####
rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  beta2
}


########################################################################
#Q1 – does removal of functional groups affect soil temperature?
  #And if so, is one more important than another?

#treatment as predictor


modTreat1 <- maxminANALYSIS %>% 
  ungroup() %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  distinct(siteID, turfID, Treatment, Temp, Precip, maxTemp, sunniness, weather, Block) %>%
  na.omit() %>% 
  #group_by(weather) %>% 
  do({
    mod <- lmer(maxTemp ~ Treatment*scale(Temp) + Treatment*sunniness + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))



modTreat1
vegCompMaxmod1 <- modTreat1 %>% 
  filter(!term %in% c("(Intercept)", "scale(Temp)", "sunniness")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!term == "scale(Temperature_level") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("Treatment", "", term),
         term = gsub("^C", "Control", term)) %>% 
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":Temp", term), "PFG x t", "PFG"))))

vegCompmod1plot  <- vegCompMaxmod1 %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(legend.title.climate, values = c(21,24, 23)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("Control:sunniness", "Control:Temp", "Control", "B:sunniness", "B:Temp", "B", "F:sunniness", "F:Temp", "F", 'G:sunniness', "G:Temp", "G", "FB:sunniness", "FB:Temp", "FB", "GB:sunniness", "GB:Temp", "GB",  "GF:sunniness", "GF:Temp", "GF"), labels = c("","C", "", '', "GF", "", "", "GB", "", "", "FB", '', "", "G", "", "", "F", "", "", "B", "")) +
  geom_vline(xintercept =  c(3.5,6.5,9.5,12.5,15.5, 18.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients", x = "Functional group")

ggsave(vegCompmod1plot, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig1.jpg", dpi = 300, width = 6, height = 4)

vegCompMaxmod1UVt <- modTreat1 %>% 
  filter(!term %in% c("(Intercept)")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!term == "scale(Temperature_level") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("Treatment", "", term),
         term = gsub("^C", "Control", term)) %>% 
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":Temp", term), "PFG x t",
                                         if_else(grepl("^sunniness$", term), "UV",
                                                 if_else(grepl("^Temp$", term), "t","PFG"))))))

vegCompMaxmod1UVt <- vegCompMaxmod1UVt %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(legend.title.climate, values = c(21, 24, 23, 22, 22)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black", "grey60", "grey60")) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("Temp", "sunniness", "Control:sunniness", "Control:Temp", "Control", "B:sunniness", "B:Temp", "B", "F:sunniness", "F:Temp", "F", 'G:sunniness', "G:Temp", "G", "FB:sunniness", "FB:Temp", "FB", "GB:sunniness", "GB:Temp", "GB",  "GF:sunniness", "GF:Temp", "GF"), labels = c("t" , "UV", "","C", "", '', "GF", "", "", "GB", "", "", "FB", '', "", "G", "", "", "F", "", "", "B", "")) +
  geom_vline(xintercept =  c(2.5,5.5,8.5,11.5,14.5, 17.5, 20.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients", x = "Functional group")

ggsave(vegCompMaxmod1UVt, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig2.jpg", dpi = 300, width = 6, height = 4)


########################################################################
#Q2 – if it seems that none is particularly more important than the others, is it purely a percent cover effect? 
#Ie, removing x% of any functional group has the same effect, regardless of the functional group?

# with cover as predictor
Cover <- maxminANALYSIS %>% 
  ungroup() %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB"),
         sTemp = scale(Temp),
         sgraminoidCov = scale(graminoidCov),
         ssunniness = scale(sunniness),
         sforbCov = scale(forbCov),
         sbryophyteCov = scale(bryophyteCov),
         svegetationHeight = scale(vegetationHeight),
         smossHeight = scale(mossHeight))

Cover <- Cover %>% 
  distinct(graminoidCov, forbCov, bryophyteCov, vegetationHeight, mossHeight, siteID, turfID, Treatment, Temp, maxTemp, sunniness, Precip, Block, Temperature_level) %>%
  na.omit()

modCover <- lmer(maxTemp ~ scale(graminoidCov)*scale(sunniness) + 
                   scale(forbCov)*scale(sunniness) + 
                   scale(bryophyteCov)*scale(sunniness) + 
                   scale(graminoidCov)*scale(Temp) + 
                   scale(forbCov)*scale(Temp) + 
                   scale(bryophyteCov)*scale(Temp) + 
                   scale(vegetationHeight)*scale(sunniness) + 
                   scale(mossHeight)*scale(sunniness) + 
                   scale(vegetationHeight)*scale(Temp) + 
                   scale(mossHeight)*scale(Temp) +
                   (1|siteID/Block), REML = FALSE, data = Cover)

coefsModCov <- modCover %>% 
  tidy() %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()


vegCompMaxmod1C <- coefsModCov %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
    if_else(grepl(":Temp", term), "PFG x t", "PFG")))) %>% 
  arrange(desc(test, .by_group = TRUE))

vegCompmod1plotCALL <- vegCompMaxmod1C %>% 
  filter(!term %in% c("Temp")) %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  scale_x_discrete(limits = c("sunniness:moss cover", "moss cover:Temp", "moss cover", "sunniness:forbCov", "forbCov:Temp", "forbCov", "graminoidCov:sunniness", "graminoidCov:Temp", "graminoidCov"), labels = c("sunniness:moss cover" = "", "sunniness:forbCov" = "", "graminoidCov:sunniness" = "", "moss cover:Temp" = "bryophyte", "forbCov:Temp" = "forb", "graminoidCov:Temp" = "graminoid", "moss cover" = "", "forbCov" = "", "graminoidCov" = "")) +
  #geom_vline(xintercept =  c(1.5, 2.5, 4.5, 5.5), colour = "grey90") +
  geom_vline(xintercept =  c(3.5, 6.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())

ggsave(vegCompmod1plotCALL, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3A.jpg", dpi = 300, width = 6, height = 4)


vegCompMaxmod1CUVt <- modCover %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%  
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":Temp", term), "PFG x t",
                                         if_else(grepl("^sunniness$", term), "UV",
                                                 if_else(grepl("^Temp$", term), "t","PFG")))))) %>% 
  arrange(desc(test, .by_group = TRUE))

vegCompMaxmod1CUVt <- vegCompMaxmod1CUVt %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black", "grey60", "grey60")) +
  scale_shape_manual(legend.title.climate, values = c(21, 24, 23, 22, 22)) +
  scale_x_discrete(limits = c("Temp", "sunniness", "sunniness:moss cover", "moss cover:Temp", "moss cover", "sunniness:forbCov", "forbCov:Temp", "forbCov", "graminoidCov:sunniness", "graminoidCov:Temp", "graminoidCov"), labels = c("t", "UV" , "", "bryophyte", "", "", "forb", "", "", "graminoid", "")) +
  #geom_vline(xintercept =  c(1.5, 2.5, 4.5, 5.5), colour = "grey90") +
  geom_vline(xintercept =  c(2.5, 5.5, 8.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())

ggsave(vegCompMaxmod1CUVt, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig3.jpg", dpi = 300, width = 6, height = 4)

#######################
#c. height as predictor
#######################

modHeight <- maxminANALYSIS %>%
  ungroup() %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  distinct(vegetationHeight, mossHeight, siteID, turfID, Treatment, Temp, maxTemp, sunniness, Precip, Block) %>%
  na.omit() %>% 
  do({
    mod <- lmer(maxTemp ~ vegetationHeight*sunniness + mossHeight*sunniness + vegetationHeight*scale(Temp) + mossHeight*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

vegCompMaxmod1H <- modHeight %>% 
  filter(!term == "(Intercept)", 
        !term == "scale(Temp)",
        !term == "sunniness") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%  
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":Temp", term), "PFG x t", "PFG")))) %>% 
  arrange(desc(test, .by_group = TRUE))

vegCompmod1plotHALL <- vegCompMaxmod1H %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  scale_x_discrete(limits = c("sunniness:moss height", "moss height:Temp", "moss height", "vegetationHeight:sunniness", "vegetationHeight:Temp", "vegetationHeight"), labels = c("sunniness:moss height" = "", "vegetationHeight:sunniness" = "", "moss height:Temp" = "non-vascular", "vegetationHeight:Temp" = "vascular", "moss height" = "", "vegetationHeight" = "")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept =  3.5, colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(vegCompmod1plotHALL, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/fig3B.jpg", dpi = 300, width = 6, height = 4)

vegCompMaxmod1HUVt <- modHeight %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>% 
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":Temp", term), "PFG x t", 
                                         if_else(grepl("^sunniness$", term), "UV",
                                                 if_else(grepl("^Temp$", term), "t", "PFG")))))) %>% 
  arrange(desc(test, .by_group = TRUE))

vegCompMaxmod1HUVt <- vegCompMaxmod1HUVt %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black", "grey60","grey60")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23, 22, 22)) +
  scale_x_discrete(limits = c("Temp", "sunniness", "sunniness:moss height", "moss height:Temp", "moss height", "vegetationHeight:sunniness", "vegetationHeight:Temp", "vegetationHeight"), labels = c("t", "UV","", "non-vascular", "", "", "vascular", "")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept =  c(2.5, 5.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(vegCompMaxmod1HUVt, file = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/supFig4.jpg", dpi = 300, width = 6, height = 4)


########################################
########## soil freezing ###############

climate <- maxmin %>% 
  select(siteID, Temperature_level, Precipitation_level, Temp, Precip)

FD <- vegComp %>%
  ungroup() %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-29")), !Treatment == "temp200cm", !is.na(Value)) %>%
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  filter(Treatment %in% c("C", "aFGB", "GB", "GF", "FB")) %>% 
  #filter(!turfID %in% c("Lav1GF", "Lav4GF", "Lav2GF")) %>% 
  group_by(date, Treatment, siteID, Temperature_level, Precipitation_level, blockID) %>% 
  summarise(minTemp = min(Value)) %>% 
  mutate(x = minTemp < 0) %>%
  arrange(date) %>% 
  group_by(Treatment, siteID) %>% 
  mutate(sum = cumsum(x), n = n()) %>%
  ungroup() %>% 
  filter(date == ymd("2016-05-30")) %>% 
  left_join(climate) %>% 
  distinct(Treatment, siteID, Temperature_level, Precipitation_level, blockID, sum, Temp, Precip) %>% 
  na.omit()

modFD <- lmer(sum ~ Treatment*scale(Temp)*scale(Precip) + (1|blockID), REML = FALSE, data = FD)

P85 <- FD %>% mutate(Precip = Precip*1.17)
t85 <- FD %>% mutate(Temp = Temp + 3.9)
tP85 <- FD %>% mutate(Temp = Temp + 3.9,
                         Precip = Precip*1.17)

FD$modPreds <- predict(modFD, re.form = ~1|blockID)
FD$P85 <-  predict(modFD, newdata = P85, re.form = ~1|blockID)
FD$t85 <-  predict(modFD, newdata = t85, re.form = ~1|blockID)
FD$tP85 <-  predict(modFD, newdata = tP85, re.form = ~1|blockID)

pal1 <- wes_palette(7, name = "Darjeeling2", type = "continuous")

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

#treatment
maxAnomPlot <- maxminAnom %>% 
  filter(!between(date, ymd("2015-09-01"), ymd("2016-05-31"))) %>%
  mutate(year = year(date)) %>% 
  filter(weather == "sunny") %>% #, !Treatment %in% c("FGB", "B", "G", "F")
  distinct(siteID, turfID, Treatment, Temp, Precip, maxAnom, sunniness, weather, Block, Temperature_level, Precipitation_level) %>%
  ggplot(aes(x = Treatment, y = maxAnom, colour = Treatment, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) +
  #geom_bar(stat = "summary", alpha = 0.8) +
  scale_x_discrete(limits = c("GF", "GB", "FB", "G", "F", "B", "C"), labels = c("B", "F", "G", "FB", "GB", "GF", "FGB")) +
  scale_colour_manual("Functional groups",values = cbPalette[c(7,5,6,2,4,10,1)], limits = c("GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bryophytes", "Forbs","Graminoids", "Bryophytes and forbs", "Graminoids and bryophytes", "Forbs and graminoids", "Bryophytes, forbs \nand graminoids")) +
  scale_fill_manual("Functional groups", values = cbPalette[c(7,5,6,2,4,10,1)], limits = c("GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bryophytes", "Forbs","Graminoids", "Bryophytes and forbs", "Graminoids and bryophytes", "Forbs and graminoids", "Bryophytes, forbs \nand graminoids")) +
  facet_grid(.~ Precipitation_level) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  labs(y = "temperature anomaly from bare ground") +
  theme(axis.title.x = element_blank())

ggsave(maxAnomPlot, file = "~/Documents/seedclimComm/figures/maxAnomPlot1.jpg", dpi = 300, width = 10, height = 4)



#cover
coverPlot1 <- maxminAnom %>% 
  #filter(between(date, ymd("2015-08-01"), ymd("2015-09-30"))) %>%
  filter(!is.na(bryophyteCov)) %>% 
  gather(key = response, value = value, graminoidCov, vegetationHeight, forbCov, mossHeight, bryophyteCov) %>% 
  mutate(response = factor(response, levels = c("graminoidCov", "forbCov", "bryophyteCov", "vegetationHeight", "mossHeight"))) %>% 
  filter(response %in% c("graminoidCov", "forbCov", "bryophyteCov")) %>% 
  ggplot(aes(x = value, y = maxAnom)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  stat_summary(geom = "point", fun.y = "mean", alpha = 0.7) +
  #scale_colour_manual(values = c("black", "grey60")) +
  #scale_fill_manual(values = c("black", "grey60")) +
  #facet_wrap( ~ Temperature_level, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "temperature anomaly from bare ground",
       x = "sunniness") +
  facet_grid(.~response)

#ggsave(coverPlot1, file = "~/Documents/seedclimComm/figures/coverPlot2.jpg", dpi = 300, width = 9, height = 5)

heightPlot1 <- maxmin %>% 
  #filter(between(date, ymd("2015-08-01"), ymd("2015-09-30"))) %>%
  filter(Treatment %in% c("C", "B", "G", "F"), weather == "sunny") %>% 
  gather(key = response, value = value, bryophyteCov, graminoidCov, vegetationHeight) %>% 
  #filter(response %in% c("bryophyteCov"), value < 580) %>% 
  ggplot(aes(x = value, y = maxTemp, colour = Treatment, fill = Treatment, group = Treatment)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_summary(geom = "point", fun.y = "mean", alpha = 0.7) +
  scale_colour_manual("", values = cbPalette) +
  scale_fill_manual("", values = cbPalette) +
  facet_grid(Temperature_level~response, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("t") +
  xlab("vegetation height (mm)") +
  theme(axis.title.y = element_text(colour = "white"))


maxmin %>% filter(Temperature_level == 6.5, maxTemp < 3) %>% View()


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


# see https://cms.met.no/site/2/klimaservicesenteret/klima-i-norge-2100/_attachment/11592?_ts=15c10419731 page 17 for predictions

#### maximum temperatures ####
treat <- maxminANALYSIS %>% 
  ungroup() %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB"),
         sTemp = scale(Temp),
         ssunniness = scale(sunniness)) %>% 
  distinct(siteID, turfID, Treatment, Temp, maxTemp, minTemp, sunniness, Precip, Block, Temperature_level) %>%
  filter(Treatment %in% c("aFGB", "C", "FB", "GB", "GF")) %>% 
  na.omit()

modTreat <- lmer(maxTemp ~ Treatment*scale(sunniness) + 
                   Treatment*scale(Temp) + 
                   Treatment*scale(Precip) +
                   (1|siteID/Block), REML = FALSE, data = treat)

P85 <- treat %>% mutate(Precip = Precip*1.17)
t85 <- treat %>% mutate(Temp = Temp + 3.9)
modSun <- treat %>% mutate(sunniness = sunniness*0.9)
tP85 <- treat %>% mutate(Temp = Temp + 3.9,
                         Precip = Precip*1.17)
tPSun85 <- treat %>% mutate(Temp = Temp + 3.9,
                         Precip = Precip*1.17,
                         sunniness = sunniness*0.9)

treat$modPreds <- predict(modTreat, re.form = ~1|siteID/Block)
treat$P85 <-  predict(modTreat, newdata = P85, re.form = ~1|siteID/Block)
treat$t85 <-  predict(modTreat, newdata = t85, re.form = ~1|siteID/Block)
treat$tP85 <-  predict(modTreat, newdata = tP85, re.form = ~1|siteID/Block)
treat$modSun <-  predict(modTreat, newdata = modSun, re.form = ~1|siteID/Block)
treat$tPSun85 <-  predict(modTreat, newdata = tPSun85, re.form = ~1|siteID/Block)

# single remainers, plus sunnines*temp
treat %>% gather(maxTemp, modPreds, modSun, P85, t85, tP85, tPSun85, key = Model, value = value) %>% 
  #filter(Temperature_level == 6.5) %>% 
  ggplot(aes(x = Treatment, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  #geom_vline(xintercept = c(3.5, 7.5), linetype = "dashed", colour = "grey60") +
  facet_grid(Temperature_level~Model)
#stat_summary(fun.y = mean, geom = "boxplot", size = 1)

pal1 <- wes_palette(7, name = "Darjeeling2", type = "continuous")

maxTempPredictions <- treat %>% gather(maxTemp, modPreds, modSun, P85, t85, tP85, tPSun85, key = Model, value = value) %>% 
  ggplot(aes(x = Temperature_level, y = value, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(.~Model) +
  scale_color_manual(values = pal1)
ggsave(maxTempPredictions, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/maxTempPredictions.jpg", dpi = 300, width = 13, height = 6)



#### minimum temperature ####
treatmin <- treat
modTreatmin <- lmer(minTemp ~ Treatment*scale(sunniness) + 
                   Treatment*scale(Temp) + 
                   Treatment*scale(Precip) +
                   (1|siteID/Block), REML = FALSE, data = treatmin)

P85 <- treatmin %>% mutate(Precip = Precip*1.17)
t85 <- treatmin %>% mutate(Temp = Temp + 3.9)
modSun <- treatmin %>% mutate(sunniness = sunniness*0.9)
tP85 <- treatmin %>% mutate(Temp = Temp + 3.9,
                         Precip = Precip*1.17)
tPSun85 <- treatmin %>% mutate(Temp = Temp + 3.9,
                            Precip = Precip*1.17,
                            sunniness = sunniness*0.9)

treatmin$modPreds <- predict(modTreatmin, re.form = ~1|siteID/Block)
treatmin$P85 <-  predict(modTreatmin, newdata = P85, re.form = ~1|siteID/Block)
treatmin$t85 <-  predict(modTreatmin, newdata = t85, re.form = ~1|siteID/Block)
treatmin$tP85 <-  predict(modTreatmin, newdata = tP85, re.form = ~1|siteID/Block)
treatmin$modSun <-  predict(modTreatmin, newdata = modSun, re.form = ~1|siteID/Block)
treatmin$tPSun85 <-  predict(modTreatmin, newdata = tPSun85, re.form = ~1|siteID/Block)

pal1 <- wes_palette(7, name = "Darjeeling2", type = "continuous")

minTempPredictions <- treatmin %>% gather(minTemp, modPreds, modSun, P85, t85, tP85, tPSun85, key = Model, value = value) %>% 
  ggplot(aes(x = Temperature_level, y = value, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(.~Model) +
  scale_color_manual(values = pal1)

ggsave(minTempPredictions, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/minTempPredictions.jpg", dpi = 300, width = 13, height = 6)


#### temperature amplitude ####
treat <- maxminANALYSIS %>% 
  ungroup() %>% 
  mutate(tempAmp = maxTemp - minTemp,
         Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB"),
         sTemp = scale(Temp),
         ssunniness = scale(sunniness)) %>% 
  distinct(siteID, turfID, Treatment, tempAmp, Temp, maxTemp, minTemp, sunniness, Precip, Block, Temperature_level) %>%
  filter(Treatment %in% c("aFGB", "C", "FB", "GB", "GF")) %>% 
  na.omit()

modTreat <- lmer(tempAmp ~ Treatment*scale(sunniness) + 
                   Treatment*scale(Temp) + 
                   Treatment*scale(Precip) +
                   (1|siteID/Block), REML = FALSE, data = treat)

P85 <- treat %>% mutate(Precip = Precip*1.17)
t85 <- treat %>% mutate(Temp = Temp + 3.9)
modSun <- treat %>% mutate(sunniness = sunniness*0.9)
tP85 <- treat %>% mutate(Temp = Temp + 3.9,
                         Precip = Precip*1.17)
tPSun85 <- treat %>% mutate(Temp = Temp + 3.9,
                            Precip = Precip*1.17,
                            sunniness = sunniness*0.9)

treat$modPreds <- predict(modTreat, re.form = ~1|siteID/Block)
treat$P85 <-  predict(modTreat, newdata = P85, re.form = ~1|siteID/Block)
treat$t85 <-  predict(modTreat, newdata = t85, re.form = ~1|siteID/Block)
treat$tP85 <-  predict(modTreat, newdata = tP85, re.form = ~1|siteID/Block)
treat$modSun <-  predict(modTreat, newdata = modSun, re.form = ~1|siteID/Block)
treat$tPSun85 <-  predict(modTreat, newdata = tPSun85, re.form = ~1|siteID/Block)

# single remainers, plus sunnines*temp
treat %>% gather(tempAmp, modPreds, modSun, P85, t85, tP85, tPSun85, key = Model, value = value) %>% 
  #filter(Temperature_level == 6.5) %>% 
  ggplot(aes(x = Treatment, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  #geom_vline(xintercept = c(3.5, 7.5), linetype = "dashed", colour = "grey60") +
  facet_grid(Temperature_level~Model)
#stat_summary(fun.y = mean, geom = "boxplot", size = 1)

pal1 <- wes_palette(7, name = "Darjeeling2", type = "continuous")

tempAmpPredictions <- treat %>% gather(tempAmp, modPreds, modSun, P85, t85, tP85, tPSun85, key = Model, value = value) %>% 
  ggplot(aes(x = Temperature_level, y = value, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(.~Model) +
  scale_color_manual(values = pal1)
ggsave(tempAmpPredictions, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 2/figures/maxTempPredictions.jpg", dpi = 300, width = 13, height = 6)
