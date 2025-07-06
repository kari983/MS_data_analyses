s# load the packages
library(readxl)
library(vegan)
library(ggplot2)
library(emmeans)
library(multcomp)
library(reshape2)
library(dplyr)
library(nlme)
library(ggpubr)
library(car)

#delete all the variables that are there in the environment
#rm(list=ls()) 

#Load combined site data
combine_elev.biomass_data <- read_excel("datasets/Combine_Sites_Biomass_2023.xlsx",
                                        sheet="combine.site.biomass")

################################################################################################################################
#Alien woody plants at 700m (based on Biomass)
################################################################################################################################
#Biomass of alien WP at 700m
Elev.700m_biomass_Alien <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Blocks, Treatments) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()


#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_Elev.700m_biomass_Alien <- lme(prop_logit ~ Treatments, random= ~1|Blocks, data= Elev.700m_biomass_Alien)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.700m_biomass_Alien, type = "pearson")^2/ df.residual(mod_Elev.700m_biomass_Alien))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_biomass_Alien, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_biomass_Alien, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_Elev.700m_biomass_Alien)
anova(mod_Elev.700m_biomass_Alien)

#estimated means (Post Hoc)
emm.Elev.700m_biomass_Alien <- emmeans(mod_Elev.700m_biomass_Alien, specs = ~ Treatments)

#Mean Groupings 
#cldisplay_numba_biom_Alien <- cld(emm.numba_biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_numba_biom_Alien 

#Explicit contrast
C_Alien    <-   c(1,0,0,0)
I_Alien    <-   c(0,1,0,0)
W_Alien    <-   c(0,0,1,0)
WI_Alien   <-   c(0,0,0,1)


contrast_list_1 <- list("C_Alien  - I_Alien"      = C_Alien  - I_Alien,
                        "C_Alien  - W_Alien"      = C_Alien  - W_Alien,
                        "C_Alien  - WI_Alien"     = C_Alien  - WI_Alien)

post_hoc_Elev.700m_biomass_Alien <- contrast(emm.Elev.700m_biomass_Alien, method = contrast_list_1)
post_hoc_Elev.700m_biomass_Alien

#Error bar plot with pairwise comparison
gg1 <- ggplot(Elev.700m_biomass_Alien) +
  aes(x = Treatments, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="logit (Alien WP biomass)") + ggtitle("700m") + coord_cartesian(ylim = c(-7.7,3.2)) +
  geom_bracket(
    xmin = c(1,1), xmax = c(2,4),
    y.position = c(1.8,2.9), label = c("**","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 0.5, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;gg1

################################################################################################################################
#Alien woody plants at 1700m (based on Biomass)
################################################################################################################################
#Biomass of alien WP at 1700m
Elev.1700m_biomass_Alien <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Blocks, Treatments) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_Elev.1700m_biomass_Alien <- lme(prop_logit ~ Treatments, random= ~1|Blocks, data= Elev.1700m_biomass_Alien)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.1700m_biomass_Alien, type = "pearson")^2/ df.residual(mod_Elev.1700m_biomass_Alien))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_biomass_Alien, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_biomass_Alien, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_Elev.1700m_biomass_Alien)
anova(mod_Elev.1700m_biomass_Alien)

#estimated means (Post Hoc)
emm.Elev.1700m_biomass_Alien <- emmeans(mod_Elev.1700m_biomass_Alien, specs = ~ Treatments)

#Mean Groupings 
#cldisplay_yawan_biom_Alien <- cld(emm.yawan_biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_yawan_biom_Alien 

#Explicit contrast
C_Alien    <-   c(1,0,0,0)
I_Alien    <-   c(0,1,0,0)
W_Alien    <-   c(0,0,1,0)
WI_Alien   <-   c(0,0,0,1)


contrast_list_2 <- list("C_Alien  - I_Alien"      = C_Alien  - I_Alien,
                        "C_Alien  - W_Alien"      = C_Alien  - W_Alien,
                        "C_Alien  - WI_Alien"     = C_Alien  - WI_Alien)

post_hoc_Elev.1700m_biomass_Alien <- contrast(emm.Elev.1700m_biomass_Alien, method = contrast_list_2)
post_hoc_Elev.1700m_biomass_Alien


#Error bar plot with pairwise comparison
gg2 <- ggplot(Elev.1700m_biomass_Alien) +
  aes(x = Treatments, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="logit (Alien WP biomass)") + ggtitle("1700m") + coord_cartesian(ylim = c(-7.7,3.2)) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  geom_bracket(
    xmin = c(1), xmax = c(4),
    y.position = c(1.8), label = c("*"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 0.5, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;gg2

################################################################################################################################
#Alien woody plants at 700m (based on Richness)
################################################################################################################################
#Richness of alien WP at 700m
Elev.700m_rich_Alien <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>% 
  summarise(Biomass = sum(Biomass_kg))

#Richness dcast
Elev.700m_rich_Alien_spp  <- Elev.700m_rich_Alien %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

#Replacing NAs with zeroes
Elev.700m_rich_Alien_spp[is.na(Elev.700m_rich_Alien_spp)] <- 0 #removing NAs
Elev.700m_rich_Alien_spp

#Richness
Elev.700m_rich_Alien_spp$alien_richness <- specnumber(Elev.700m_rich_Alien_spp[,3:21]) #Number of plant species 
Elev.700m_rich_Alien_spp

#Proportion of Alien plants
Elev.700m_rich_Alien_spp$Alien_rich_prop <- Elev.700m_rich_Alien_spp$alien_richness / sum(Elev.700m_rich_Alien_spp$alien_richness)
Elev.700m_rich_Alien_spp

#Proportion of Native plants
Elev.700m_rich_Alien_spp$Native_rich_prop <- 1 - Elev.700m_rich_Alien_spp$Alien_rich_prop
Elev.700m_rich_Alien_spp

#Logit of proportion of Native plants
Elev.700m_rich_Alien_spp$Native_rich_LogitProp <- log(Elev.700m_rich_Alien_spp$Native_rich_prop)
Elev.700m_rich_Alien_spp

#Logit of proportion of Alien plants
Elev.700m_rich_Alien_spp$Alien_rich_LogitProp <- log(Elev.700m_rich_Alien_spp$Alien_rich_prop)
Elev.700m_rich_Alien_spp

#----------------------------------------------------------------------------------------------------------------------
#Model
mod_Elev.700m_rich_Alien_spp <- lme(Alien_rich_LogitProp ~ Treatments, random = ~1|Blocks, data = Elev.700m_rich_Alien_spp)

# Summary and Anova
summary(mod_Elev.700m_rich_Alien_spp)
anova(mod_Elev.700m_rich_Alien_spp)

#estimated means (Post Hoc)
emm.Elev.700m_rich_Alien_spp <- emmeans(mod_Elev.700m_rich_Alien_spp, specs = ~ Treatments)

#Mean Groupings 
#cldisplay_numba_rich_Alien <- cld(emm.numba_rich_Alien, Letters = letters,  alpha = 0.05)

#Explicit contrast
C_Alien    <-   c(1,0,0,0)
I_Alien    <-   c(0,1,0,0)
W_Alien    <-   c(0,0,1,0)
WI_Alien   <-   c(0,0,0,1)


contrast_list_3 <- list("C_Alien  - I_Alien"      = C_Alien  - I_Alien,
                        "C_Alien  - W_Alien"      = C_Alien  - W_Alien,
                        "C_Alien  - WI_Alien"     = C_Alien  - WI_Alien)

post_hoc_Elev.700m_rich_Alien_spp <- contrast(emm.Elev.700m_rich_Alien_spp, method = contrast_list_3)
post_hoc_Elev.700m_rich_Alien_spp

#Error bar plot with pairwise comparison
gg3 <- ggplot(Elev.700m_rich_Alien_spp) +
  aes(x = Treatments, y = Alien_rich_LogitProp) + 
  geom_jitter(size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="logit (Alien WP richness)") + ggtitle("700m") + ylim(-4.7,-2.7) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  geom_bracket(
      xmin = c(1), xmax = c(2),
      y.position = c(-3.1), label = c("*"),label.size = 7,
      tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 0.5, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;gg3

################################################################################################################################
#Alien woody plants at 1700m (based on Richness)
################################################################################################################################
#Richness of alien WP at 1700m
Elev.1700m_biomass_Alien <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>% 
  summarise(Biomass = sum(Biomass_kg))

#Richness dcast
Elev.1700m_biomass_Alien_spp  <- Elev.1700m_biomass_Alien %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

#Replacing NAs with zeroes
Elev.1700m_biomass_Alien_spp[is.na(Elev.1700m_biomass_Alien_spp)] <- 0 #removing NAs
Elev.1700m_biomass_Alien_spp

#Richness
Elev.1700m_biomass_Alien_spp$alien_richness <- specnumber(Elev.1700m_biomass_Alien_spp[,3:12]) #Number of plant species 
Elev.1700m_biomass_Alien_spp

#Proportion of Alien plants
Elev.1700m_biomass_Alien_spp$alien_rich_prop <- Elev.1700m_biomass_Alien_spp$alien_richness / sum(Elev.1700m_biomass_Alien_spp$alien_richness)
Elev.1700m_biomass_Alien_spp

#Proportion of Native plants
Elev.1700m_biomass_Alien_spp$native_rich_prop <- 1 - Elev.1700m_biomass_Alien_spp$alien_rich_prop 
Elev.1700m_biomass_Alien_spp

#Logit of proportion of Alien plants
Elev.1700m_biomass_Alien_spp$alien_rich_LogitProp <- log(Elev.1700m_biomass_Alien_spp$alien_rich_prop)
Elev.1700m_biomass_Alien_spp

#Logit of proportion of Native plants
Elev.1700m_biomass_Alien_spp$native_rich_LogitProp <- log(Elev.1700m_biomass_Alien_spp$native_rich_prop)
Elev.1700m_biomass_Alien_spp

#----------------------------------------------------------------------------------------------------------------------
#Model
mod_Elev.1700m_biomass_Alien_spp <- lme(alien_rich_LogitProp ~ Treatments, random=~1|Blocks, data = Elev.1700m_biomass_Alien_spp)

#check_homogeneity of variances
#performance::check_homogeneity(mod_yawan_rich_Alien)

# Summary and Anova
summary(mod_Elev.1700m_biomass_Alien_spp)
anova(mod_Elev.1700m_biomass_Alien_spp)

#estimated means (Post Hoc)
emm.Elev.1700m_biomass_Alien_spp <- emmeans(mod_Elev.1700m_biomass_Alien_spp, specs = ~ Treatments)

#Mean Groupings 
#cldisplay_yawan_rich_Alien <- cld(emm.yawan_rich_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_yawan_rich_Alien

#Explicit contrast
C_Alien    <-   c(1,0,0,0)
I_Alien    <-   c(0,1,0,0)
W_Alien    <-   c(0,0,1,0)
WI_Alien   <-   c(0,0,0,1)

contrast_list_4 <- list("C_Alien  - I_Alien"      = C_Alien  - I_Alien,
                        "C_Alien  - W_Alien"      = C_Alien  - W_Alien,
                        "C_Alien  - WI_Alien"     = C_Alien  - WI_Alien)

post_hoc_Elev.1700m_biomass_Alien_spp <- contrast(emm.Elev.1700m_biomass_Alien_spp, method = contrast_list_4)
post_hoc_Elev.1700m_biomass_Alien_spp

#Error bar plot with pairwise comparison
gg4 <- ggplot(Elev.1700m_biomass_Alien_spp) +
  aes(x = Treatments, y = alien_rich_LogitProp) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="logit (Alien WP richness)") + ggtitle("1700m") + ylim(-4.7,-2.7) +
  geom_bracket(
      xmin = c(1), xmax = c(3),
      y.position = c(-3), label = c("*"),label.size = 7,
      tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 0.5, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;gg4


#-----------------------------------------------------------------------------------------------------------------
#COMBINE PLOTS
#ALL plots combine
AlienNativePlotWP <- ggarrange(
                    gg1 + rremove("xlab"), 
                    gg3 + rremove("xlab"), 
                    gg2, 
                    gg4,# remove axis labels from plots
                    labels = c('A', 'B','C','D'),
                    ncol = 2, nrow = 2,
                    common.legend = FALSE, legend = "none",
                    align = "hv", 
                    font.label = list(size = 16, color = "black", face = "bold", family = NULL, position = "top")); AlienNativePlotWP


#Saving ordination plot
#ggsave("AlienNativePlotWP.jpg", width = 19, height = 21.5, units = "cm")

#Saving ordination plot in tiff format (dpi = 600)
ggsave("AlienNativePlotWP.tiff", width = 20, height = 22, units = "cm", dpi = 600)

