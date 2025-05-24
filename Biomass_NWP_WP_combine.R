# load the packages
library(readxl)
library(ggplot2)
library(emmeans)
library(multcomp)
library(reshape2)
library(dplyr)
library(nlme)
library(ggpubr)

#clear the environment
rm(list=ls()) 


######################################################################################################
##############################################################################################################
#COMBINED BIOMASS
########################################################################################
#Combined biomass for NWP and WP in Numba (700m)

# load the data
numba_biomass_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                                 sheet="Numba_biomass_2023")

#(A) Combined biomass in numba (700m)
numba_total_biomass <- numba_biomass_data %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_numba_total_biomass <- lme(log(Biomass) ~ Treatments, random= ~1| Gardens, 
                               data= numba_total_biomass) 

#Check for overdispersion
dispersion <- sum(residuals(mod_numba_total_biomass, type = "pearson")^2/ df.residual(mod_numba_total_biomass))
dispersion 


## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_total_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_total_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_numba_total_biomass)
anova(mod_numba_total_biomass, type="marginal")

#estimated means (Post Hoc)
emm.total_numba_biom <- emmeans(mod_numba_total_biomass, specs = ~ Treatments)

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list1 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test1 <- contrast(emm.total_numba_biom, method = contrast_list1)
post_hoc_test1

#Errorbars with pairwise comparison
p1_numba_total_biom <- ggplot(numba_total_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="ln (Total Biomass in kg)") + ggtitle("700m") + ylim(-2,6) +
  geom_bracket(
    xmin = c("C"), xmax = c("I"),
    y.position = c(5.5), label = c("*"),label.size = 7,
    tip.length = 0.0,colour = "blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p1_numba_total_biom 

##############################################################################################################
#(B) Non-woody Biomass in Numba (700m)

#Non-Woody data
numba_NonWoody_biomass <- numba_biomass_data %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))


#Model
mod_numba_NW_biomass <- lme(log(Biomass) ~ Treatments, random= ~1| Gardens, data= numba_NonWoody_biomass) 

#Check for overdispersion
dispersion <- sum(residuals(mod_numba_NW_biomass, type = "pearson")^2/ df.residual(mod_numba_NW_biomass))
dispersion 


## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_NW_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_NW_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_numba_NW_biomass)
anova(mod_numba_NW_biomass)

#estimated means (Post Hoc)
emm.numba_NW_biom <- emmeans(mod_numba_NW_biomass , specs = ~ Treatments)

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list3 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test3 <- contrast(emm.numba_NW_biom, method = contrast_list3)
post_hoc_test3


#Errorbars with pairwise comparison
p1_numba_NW_biomass <- ggplot(numba_NonWoody_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="ln (NWP Biomass in kg)") + ggtitle("700m") + ylim(-2,6) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","W"),
    y.position = c(4.7,5.8), label = c("*","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p1_numba_NW_biomass


##########################################################################
#(C) Woody Biomass by in Numba (700m)
numba_Woody_biomass <- numba_biomass_data %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_numba_W_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Gardens, 
                           data= numba_Woody_biomass)  

#Check for overdispersion
dispersion <- sum(residuals(mod_numba_W_biomass, type = "pearson")^2/ df.residual(mod_numba_W_biomass))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_W_biomass , ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_W_biomass , ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_numba_W_biomass)
anova(mod_numba_W_biomass)

#estimated means (Post Hoc)
emm.numba_W_biom <- emmeans(mod_numba_W_biomass, specs = ~ Treatments)

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list4 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test4 <- contrast(emm.numba_W_biom , method = contrast_list4)
post_hoc_test4

#Errorbars with pairwise comparison
p1_numba_W_biomass <- ggplot(numba_Woody_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="ln (WP Biomass in kg)") + ggtitle("700m") + ylim(-2,6) +
  geom_bracket(
    xmin = c("C","C","C"), xmax = c("I","W","WI"),
    y.position = c(3.3,4.5,5.6), label = c("**","**","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p1_numba_W_biomass

#########################################################################################################################
#############################################################################################################
#Combined biomass for NWP and WP in Yawan (1700m)
#load the data
yawan_biomass_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                                 sheet = "Yawan_biomass_2023")

#(A) Combined biomass in yawan (1700m)
yawan_total_biomass <- yawan_biomass_data %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_yawan_total_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Gardens, 
                               data= yawan_total_biomass) 

#Check for overdispersion
dispersion <- sum(residuals(mod_yawan_total_biomass, type = "pearson")^2/ df.residual(mod_yawan_total_biomass))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_total_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_total_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_yawan_total_biomass)
anova(mod_yawan_total_biomass)

#estimated means (Post Hoc)
emm.yawan_total_biom <- emmeans(mod_yawan_total_biomass, specs = ~ Treatments)

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list2 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test2 <- contrast(emm.yawan_total_biom, method = contrast_list2)
post_hoc_test2

#Errorbars with pairwise comparison
p1_yawan_total_biom <- ggplot(yawan_total_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="ln (Total Biomass in kg)") + ggtitle("1700m") + ylim(-4,7) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p1_yawan_total_biom


#############################################################################################################
# Non-woody Biomass in Yawan (1700m)

#(B) Non-Woody biomass in Yawan (1700m)
yawan_NW_biomass <- yawan_biomass_data %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_yawan_NW_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Gardens, 
                            data= yawan_NW_biomass)  

#Check for overdispersion
dispersion <- sum(residuals(mod_yawan_NW_biomass, type = "pearson")^2/ df.residual(mod_yawan_NW_biomass))
dispersion 


## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_NW_biomass , ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_NW_biomass , ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_yawan_NW_biomass )
anova(mod_yawan_NW_biomass )

#estimated means (Post Hoc)
emm.yawan_NW_biomass  <- emmeans(mod_yawan_NW_biomass, specs = ~ Treatments)

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list5 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test5 <- contrast(emm.yawan_NW_biomass, method = contrast_list5)
post_hoc_test5


#Errorbars with pairwise comparison
p_yawan_NW_biomass <- ggplot(yawan_NW_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="ln (NWP Biomass in kg)") + ggtitle("1700m") +  ylim(-4,7) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(5.1,6.5), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_yawan_NW_biomass



#(C)  Woody Biomass in Yawan (1700m)
yawan_W_biomass <- yawan_biomass_data %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_yawan_W_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Gardens, 
                           data= yawan_W_biomass)

#Check for overdispersion
dispersion <- sum(residuals(mod_yawan_W_biomass, type = "pearson")^2/ df.residual(mod_yawan_W_biomass))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_W_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_W_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_yawan_W_biomass)
anova(mod_yawan_W_biomass)

#estimated means (Post Hoc)
emm.yawan_W_biomass <- emmeans(mod_yawan_W_biomass, specs = ~ Treatments)

#Explicit contrast
contrast_list6 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test6 <- contrast(emm.yawan_W_biomass, method = contrast_list6)
post_hoc_test6

#Errorbars with pairwise comparison
p_yawan_W_biomass <- ggplot(yawan_W_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="ln (WP Biomass in kg)") + ggtitle("1700m") + ylim(-4,7) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(5.2,6.7), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_yawan_W_biomass

#combine plots for biomasses
biomass_NWP_WP_plot <- cowplot::plot_grid(p1_numba_NW_biomass,
                                          p1_numba_W_biomass,
                                          p1_numba_total_biom, 
                                          p_yawan_NW_biomass,
                                          p_yawan_W_biomass,
                                          p1_yawan_total_biom,
                                          ncol = 3, byrow = TRUE,labels = c('A', 'B','C','D','E','F'), align="hv"); biomass_NWP_WP_plot 

#Saving plot
ggsave("biomass_NWP_WP_plot.jpg", width = 25, height = 25, units = "cm")





 





