# source species composition compilation
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/vegetation/00funcab_data_processing.R")

# source trait imputation
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/vegetation/trait_imputation.R")

library(vegan)
library(TAM)

# filter for funcab-only turfs
comp2 <- comp2 %>% 
  filter(!Treatment == "XC", Year == 2017)

# create grouping variable before merge
community_cover <- comp2 %>%
  mutate(speciesID = paste0(siteID,"_", species))

# calculation of CWM and FDvar; Community weighted mean and community-weighted variance of trait values
# join imputed traits with species cover, and filter for treatment
community_FD <- left_join(community_cover, Species_traits, by = c("speciesID", "siteID")) %>%
  select(siteID, Treatment, blockID, turfID, Year, tempLevel, temp7010, temp0916, precipLevel, precip7010, precip0916, species, functionalGroup, cover, C, N, CN, SLA, Lth, LDMC, sqrtLA, logHeight) %>%
  filter(!is.na(cover), cover > 0)

# calculate community weighted means and variances
# https://rdrr.io/cran/TAM/man/weighted_Stats.html
community_FD <- community_FD %>% 
  group_by(siteID, blockID, turfID, Treatment, Year) %>%
  mutate(richness = sum(n_distinct(species))) %>%
  mutate(diversity = diversity(cover, index = "shannon")) %>%
  mutate(evenness = (diversity/log(richness))) %>% 
  group_by(siteID, blockID, turfID, Treatment, functionalGroup, tempLevel, temp7010, temp0916, precipLevel, precip7010, precip0916) %>%
  #gather(LDMC, Lth, sqrtLA, SLA, logHeight, CN, C, N, key = "trait", value = "value")
  summarise(sumcover = sum(cover),
            richness = mean(richness),
            diversity = mean(diversity),
            evenness = mean(evenness),
            Wmean_LDMC = weighted_mean(LDMC, cover),
            Wmean_Lth= weighted_mean(Lth, cover),
            Wmean_LA= weighted_mean(sqrtLA, cover),
            Wmean_SLA= weighted_mean(SLA, cover),
            Wmean_Height= weighted_mean(logHeight),
            Wmean_CN = weighted_mean(CN, cover),
            Wmean_C = weighted_mean(C, cover),
            Wmean_N = weighted_mean(N, cover),
            Wvar_LDMC= wt.var(LDMC, cover),
            Wvar_Lth = wt.var(Lth, cover),
            Wvar_LA  = wt.var(sqrtLA, cover),
            Wvar_SLA = wt.var(SLA, cover),
            Wvar_Height = wt.var(logHeight, cover),
            Wvar_C = wt.var(C, cover),
            Wvar_N = wt.var(N, cover),
            Wvar_CN = wt.var(CN, cover),
            Wkur_LDMC= weighted_kurtosis(LDMC, cover),
            Wkur_Lth = weighted_kurtosis(Lth, cover),
            Wkur_LA  = weighted_kurtosis(sqrtLA, cover),
            Wkur_SLA = weighted_kurtosis(SLA, cover),
            Wkur_Height = weighted_kurtosis(logHeight, cover),
            Wkur_C = weighted_kurtosis(C, cover),
            Wkur_N = weighted_kurtosis(N, cover),
            Wkur_CN = weighted_kurtosis(CN, cover)) %>%
  #gather(key= Trait, value= value, -c(turfID:Year))%>%
  ungroup()

# gather traits
community_FD <- community_FD %>% 
  #filter(Treatment %in% c("C", "FGB", "GF", "GB", "FB")) %>% 
  gather(c(sumcover:Wkur_CN), key = "trait", value = "value")


#--------- analyses --------------#
#load packages
library(lme4)
library(MuMIn)
library(GGally)
library(tibble)
library(broom)

# Scaling explanatory variables
# relevel treatment so that TTC is the intercept
community_FD_analysis <- community_FD %>% 
  mutate(Sprecip0916 = as.numeric(scale(precip0916)),
         Stemp0916 = as.numeric(scale(temp0916))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("C", "FGB", "GF", "GB", "FB", "G", "F", "B"))) %>% 
  group_by(trait, functionalGroup) %>% 
  mutate(value = scale(value)) %>% 
  filter(!is.na(value), !is.infinite(value))

mod1temp <- community_FD_analysis %>% 
  filter(Treatment %in% c("C", "G", "B", "GB"), functionalGroup == "forb") %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(value ~ Treatment*Stemp0916*Sprecip0916 + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  #filter(term %in% c("TTtreatRTC","TTtreatRTC:Stemp0916:SYear", "TTtreatRTC:Sprecip0916:SYear", "TTtreatRTC:SYear")) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  ungroup()

mod2temp <- community_FD_analysis %>% 
  filter(Treatment %in% c("C", "F", "B", "FB"), functionalGroup == "graminoid") %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(value ~ Treatment*Stemp0916*Sprecip0916 + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  #filter(term %in% c("TTtreatRTC","TTtreatRTC:Stemp0916:SYear", "TTtreatRTC:Sprecip0916:SYear", "TTtreatRTC:SYear")) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  ungroup()








# source plotting colours etc
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/plotting_dim.R")

community_FDPlot <- community_FD %>% 
  select(-(Wvar_LDMC:Wvar_CN)) %>% 
  group_by(turfID, trait, siteID, functionalGroup) %>% 
  left_join(community_FD %>% filter(Treatment == "C") %>% ungroup() %>% select(Cvalue = value, siteID, blockID, trait)) %>%
  mutate(valueAnom = value - Cvalue,
         char = case_when(
           Treatment %in% c("B", "F", "G") ~ "effect",
           Treatment %in% c("FB", "GF", "GB") ~ "response"
         )) %>%
  filter(!Treatment %in% c("C", "FGB"))


community_cover %>% 
  gather(mossCov, forbCov, graminoidCov, key = FG, value = value) %>% 
  group_by(turfID, siteID, FG) %>% 
  left_join(community_cover %>% filter(Treatment == "C") %>% ungroup() %>% select(Cvalue = value, siteID, blockID, trait))

#------------ PLOTTING --------------
community_FDPlot %>% ggplot(aes(x = tempLevel, y = valueAnom, colour = Treatment, linetype)) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, position = position_dodge(0.5)) +
  #stat_summary(fun.data = "mean_cl_boot", geom = "line", size = 0.8) +
  facet_wrap(~ trait, scales = "free_y") +
  geom_hline(yintercept = 0)

community_FDPlot %>% ggplot(aes(x = precipLevel, y = valueAnom, colour = Treatment, linetype)) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, position = position_dodge(130)) +
  #stat_summary(fun.data = "mean_cl_boot", geom = "line", size = 0.8) +
  facet_wrap(~ trait, scales = "free_y") +
  geom_hline(yintercept = 0)


g <- community_FDPlot %>% 
  filter(!Treatment == "GF") %>%
 # mutate(Treatment = case_when(
 #  Treatment == "FB" ~ "G",
 #  Treatment == "GB" ~ "F"
 #)) %>% 
  group_by(trait) %>% 
  mutate(scaleAnom = scale(valueAnom)) %>% 
  filter(!is.na(functionalGroup)) %>% 
  ggplot(aes(x = factor(tempLevel), y = factor(precipLevel), fill = scaleAnom)) +
  geom_tile() +
  scale_fill_gradient2(low = pal1[3], mid = "snow1", high = pal1[4]) +
  facet_grid(trait~char*Treatment*functionalGroup, scales = "free")

ggsave(g, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 3/figures/fig1.jpg", dpi = 300, height = 22.5, width = 11)



#---------- abundance/dominance analyses and figures -----------#
abund <- comp2 %>% 
  #filter(functionalGroup == "forb") %>% 
  group_by(siteID, Year, species, tempLevel, precipLevel, Treatment, functionalGroup) %>% 
  summarise(meanCov = mean(cover)) %>% 
  ungroup() %>% 
  group_by(precipLevel, tempLevel, species) %>%
  mutate(dominance = case_when(
    mean(meanCov) > 16 ~ "dominant",
    mean(meanCov)  < 16 ~ "subordinate"
  )) %>% 
  #mutate(dominance = if_else(mean(meanCov, na.rm = TRUE) > 15, "dominant", "subordinate")) %>% 
  mutate(label = if_else(dominance == "dominant", as.character(species), NA_character_))

lm1 <- lm(meanCov ~ species + precipLevel + 0, data = abund) %>% 
  tidy() %>% 
  mutate(term = gsub("species", "", term)) %>% 
  filter(!term == "precip")

lm1

abund %>%
  left_join(lm1, by = c("species" = "term")) %>% 
  filter(!Treatment %in% c("FGB", "C"), dominance == "dominant") %>% 
  ggplot(aes(x = precipLevel, y = meanCov, colour = dominance, group = species)) +
  #stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  #geom_line() +
  geom_label(aes(label = label),
             nudge_x = -0.6,
             nudge_y = -2.45,
             na.rm = TRUE) +
  scale_colour_manual(values = c("Black", "grey80")) +
  facet_grid(. ~ Treatment*functionalGroup) +
  geom_point(size = 3) +
  labs(y = "Mean cover (%)") +
  axis.dimLarge +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave(filename = paste0("fig4.jpg"), width = 11, height = 4.5, dpi = 300, path = "~/OneDrive - University of Bergen/Research/FunCaB/figures")
