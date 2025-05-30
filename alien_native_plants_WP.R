# load the packages
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


##############################################################################################################
#DATA
########################################################################################
# load the data for Numba (700m) and Yawan (1700m)
numba_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                         sheet="Numba_biomass_2023")

yawan_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                         sheet = "Yawan_biomass_2023")

################################################################################################################################
#Alien woody plants at Numba (700m): Biomass
################################################################################################################################
numba_biomass_Alien <- numba_data %>%
  filter(Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Gardens, Treatments) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_numba_biom_Alien <- lme(prop_logit ~ Treatments, random= ~1| Gardens, data= numba_biomass_Alien)

#Check for overdispersion
dispersion <- sum(residuals(mod_numba_biom_Alien, type = "pearson")^2/ df.residual(mod_numba_biom_Alien))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_biom_Alien, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_biom_Alien, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_numba_biom_Alien)
anova(mod_numba_biom_Alien)

#estimated means (Post Hoc)
emm.numba_biom_Alien <- emmeans(mod_numba_biom_Alien, specs = ~ Treatments)

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

post_hoc_numba_biom_Alien <- contrast(emm.numba_biom_Alien, method = contrast_list_1)
post_hoc_numba_biom_Alien

#Error bar plot with pairwise comparison
biom_numba_Alien <- ggplot(numba_biomass_Alien) +
  aes(x = Treatments, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="logit (Alien WP biomass)") + ggtitle("700m") + ylim(-7.7,3.2) +
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
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;biom_numba_Alien

################################################################################################################################
#Alien woody plants at Yawan (1700m):Biomass
################################################################################################################################
yawan_biomass_Alien <- yawan_data %>%
  filter(Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Gardens, Treatments) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_yawan_biom_Alien <- lme(prop_logit ~ Treatments, random= ~1| Gardens, data= yawan_biomass_Alien)

#Check for overdispersion
dispersion <- sum(residuals(mod_yawan_biom_Alien, type = "pearson")^2/ df.residual(mod_yawan_biom_Alien))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_biom_Alien, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_biom_Alien, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_yawan_biom_Alien)
anova(mod_yawan_biom_Alien)

#estimated means (Post Hoc)
emm.yawan_biom_Alien <- emmeans(mod_yawan_biom_Alien, specs = ~ Treatments)

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

post_hoc_yawan_biom_Alien <- contrast(emm.yawan_biom_Alien, method = contrast_list_2)
post_hoc_yawan_biom_Alien


#Error bar plot with pairwise comparison
biom_yawan_Alien <- ggplot(yawan_biomass_Alien) +
  aes(x = Treatments, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="") + ggtitle("1700m") + ylim(-7.7,3.2) +
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
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;biom_yawan_Alien

################################################################################################################################
#Alien woody plants at Numba (700m): Richness
################################################################################################################################
#Grouped data
numba_biomass_Alien <- numba_data %>%
  filter(Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>% 
  summarise(Biomass = sum(Biomass_kg))

#Richness dcast
numba_rich_Alien_spp  <- numba_biomass_Alien %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

#Replacing NAs with zeroes
numba_rich_Alien_spp[is.na(numba_rich_Alien_spp)] <- 0 #removing NAs
numba_rich_Alien_spp

#Richness
numba_rich_Alien_spp$alien_richness <- specnumber(numba_rich_Alien_spp[,3:22]) #Number of plant species 
numba_rich_Alien_spp

#Proportion of Alien plants
numba_rich_Alien_spp$Alien_rich_prop <- numba_rich_Alien_spp$alien_richness / sum(numba_rich_Alien_spp$alien_richness)
numba_rich_Alien_spp

#Proportion of Native plants
numba_rich_Alien_spp$Native_rich_prop <- 1 - numba_rich_Alien_spp$Alien_rich_prop
numba_rich_Alien_spp

#Logit of proportion of Native plants
numba_rich_Alien_spp$Native_rich_LogitProp <- log(numba_rich_Alien_spp$Native_rich_prop)
numba_rich_Alien_spp

#Logit of proportion of Alien plants
numba_rich_Alien_spp$Alien_rich_LogitProp <- log(numba_rich_Alien_spp$Alien_rich_prop)
numba_rich_Alien_spp

#----------------------------------------------------------------------------------------------------------------------
#Model
mod_numba_rich_Alien <- lme(Alien_rich_LogitProp ~ Treatments, random = ~1|Gardens, data = numba_rich_Alien_spp)

# Summary and Anova
summary(mod_numba_rich_Alien)
anova(mod_numba_rich_Alien)

#estimated means (Post Hoc)
emm.numba_rich_Alien <- emmeans(mod_numba_rich_Alien, specs = ~ Treatments)

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

post_hoc_numba_rich_Alien <- contrast(emm.numba_rich_Alien, method = contrast_list_3)
post_hoc_numba_rich_Alien

#Error bar plot with pairwise comparison
rich_numba_Alien <- ggplot(numba_rich_Alien_spp) +
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
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;rich_numba_Alien

################################################################################################################################
#Alien woody plants at Yawan (1700m): Richness
################################################################################################################################
#Grouped data
yawan_biomass_Alien <- yawan_data %>%
  filter(Status %in% "Alien", Plants %in% "woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>% 
  summarise(Biomass = sum(Biomass_kg))

#Richness dcast
yawan_rich_Alien_spp  <- yawan_biomass_Alien %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

#Replacing NAs with zeroes
yawan_rich_Alien_spp[is.na(yawan_rich_Alien_spp)] <- 0 #removing NAs
yawan_rich_Alien_spp

#Richness
yawan_rich_Alien_spp$alien_richness <- specnumber(yawan_rich_Alien_spp[,3:12]) #Number of plant species 
yawan_rich_Alien_spp

#Proportion of Alien plants
yawan_rich_Alien_spp$alien_rich_prop <- yawan_rich_Alien_spp$alien_richness / sum(yawan_rich_Alien_spp$alien_richness)
yawan_rich_Alien_spp

#Proportion of Native plants
yawan_rich_Alien_spp$native_rich_prop <- 1 - yawan_rich_Alien_spp$alien_rich_prop 
yawan_rich_Alien_spp

#Logit of proportion of Alien plants
yawan_rich_Alien_spp$alien_rich_LogitProp <- log(yawan_rich_Alien_spp$alien_rich_prop)
yawan_rich_Alien_spp

#Logit of proportion of Native plants
yawan_rich_Alien_spp$native_rich_LogitProp <- log(yawan_rich_Alien_spp$native_rich_prop)
yawan_rich_Alien_spp

#----------------------------------------------------------------------------------------------------------------------
#Model
mod_yawan_rich_Alien <- lme(alien_rich_LogitProp ~ Treatments, random=~1|Gardens, data = yawan_rich_Alien_spp)

#check_homogeneity of variances
#performance::check_homogeneity(mod_yawan_rich_Alien)

# Summary and Anova
summary(mod_yawan_rich_Alien)
anova(mod_yawan_rich_Alien)

#estimated means (Post Hoc)
emm.yawan_rich_Alien <- emmeans(mod_yawan_rich_Alien, specs = ~ Treatments)

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

post_hoc_yawan_rich_Alien <- contrast(emm.yawan_rich_Alien, method = contrast_list_4)
post_hoc_yawan_rich_Alien

#Error bar plot with pairwise comparison
rich_yawan_Alien <- ggplot(yawan_rich_Alien_spp) +
  aes(x = Treatments, y = alien_rich_LogitProp) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.02) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="") + ggtitle("1700m") + ylim(-4.7,-2.7) +
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
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;rich_yawan_Alien


#-----------------------------------------------------------------------------------------------------------------
#COMBINE PLOTS
#ALL plots combine
AlienNativePlotWP <- ggarrange(biom_numba_Alien + rremove("xlab"), 
                    biom_yawan_Alien + rremove("xlab"), 
                    rich_numba_Alien, 
                    rich_yawan_Alien,# remove axis labels from plots
                    labels = c('A', 'B','C','D'),
                    ncol = 2, nrow = 2,
                    common.legend = FALSE, legend = "none",
                    align = "hv", 
                    font.label = list(size = 16, color = "black", face = "bold", family = NULL, position = "top")); AlienNativePlotWP


#Saving ordination plot
#ggsave("AlienNativePlotWP.jpg", width = 19, height = 21.5, units = "cm")

#Saving ordination plot in tiff format (dpi = 600)
ggsave("AlienNativePlotWP.tiff", width = 20, height = 22, units = "cm", dpi = 600)

