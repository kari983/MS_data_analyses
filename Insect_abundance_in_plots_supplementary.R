#load the packages
library(readxl)
library(ggplot2)
library(emmeans)
library(nlme)
library(car)
library(ggpubr)
library(dplyr)

#delete all the variables that are there in the environment
#rm(list=ls()) 

##########################################################################################################################################
#Total Insect abundance for separate sites
#################################################################################################
#load data
insect_data1 <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/Manuscript/Finalized version/Final_MS_R_codes/datasets/CombineSite_Insects_Orders_data.xlsx",
                           sheet="combine_insect_abundance")

#---------------------------------------------------
#Insect abundance at 700m 
Elev700m_insect_data  <- insect_data1  %>%
  filter(Elev %in% "700m") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Bartlett test for equality of variances
bartlett.test(Abundance.sum ~ Treatments, data = Elev700m_insect_data) #variance is different for treatments

## ## Model 
mod_Elev700m_insect_abundance <- lme(Abundance.sum ~ Treatments, random= ~1|Blocks, data= Elev700m_insect_data,
    weights = varIdent(form = ~ 1|Treatments))
#Since variances based on biomass differs among treatments, we used the expression ~ 1|Treatments as a one-sided 
#formula to show that the variance differs between the levels of Treatments.


summary(mod_Elev700m_insect_abundance) 
anova(mod_Elev700m_insect_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev700m_insect_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev700m_insect_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.Elev700m_insect_abundance = emmeans(mod_Elev700m_insect_abundance, specs = ~ Treatments, adjust="BH")

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list_1 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_Elev700m_insect_abundance <- contrast(emm.Elev700m_insect_abundance, method = contrast_list_1)
post_Elev700m_insect_abundance

#Errorbar plot
g1 <- ggplot(Elev700m_insect_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.4) +
  labs(x="") + labs (y="Total insect abundance") + ggtitle("700m") + coord_cartesian(ylim = c(0,1600)) +
  geom_bracket(
            xmin = c("C","C"), xmax = c("I","WI"),
            y.position = c(600,900), label = c("***","***"),label.size = 7,
            tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); g1

#---------------------------------------------------
#Insect abundance at 1700m
Elev1700m_insect_data  <- insect_data1  %>%
  filter(Elev %in% "1700m") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Bartlett test for equality of variances
bartlett.test(Abundance.sum ~ Treatments, data = Elev1700m_insect_data) #variance is different for treatments


## ## Model 
mod_Elev1700m_insect_abundance <- lme(Abundance.sum ~ Treatments, random= ~1|Blocks, data= Elev1700m_insect_data,
                                     weights = varIdent(form = ~ 1|Treatments))
#Since variances based on biomass differs among treatments, we used the expression ~ 1|Treatments as a one-sided 
#formula to show that the variance differs between the levels of Treatments.

summary(mod_Elev1700m_insect_abundance) 
anova(mod_Elev1700m_insect_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev1700m_insect_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev1700m_insect_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.Elev1700m_insect_abundance = emmeans(mod_Elev1700m_insect_abundance, specs = ~ Treatments, adjust="BH")

#Explicit contrasts
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list_2 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_Elev1700m_insect_abundance <- contrast(emm.Elev1700m_insect_abundance, method = contrast_list_2)
post_Elev1700m_insect_abundance

#Errorbar plot
g2 <- ggplot(Elev1700m_insect_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.4) +
  labs(x="Treatments") + labs (y="Total insect abundance") + ggtitle("1700m") + coord_cartesian(ylim = c(0,1600)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(600,950), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;g2

##########################################################################################################################################
#Herbivore abundance
#################################################################################################
#load data
insect_data_herbivore <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/Manuscript/Finalized version/Final_MS_R_codes/datasets/CombineSite_Insects_Orders_data.xlsx",
                           sheet="herbivores")

#---------------------------------------------------
#Herbivore abundance at 700m
Elev700m_herbivore_data  <- insect_data_herbivore %>%
  filter(Elev %in% "700m") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Bartlett test for equality of variances
bartlett.test(Abundance.sum ~ Treatments, data = Elev700m_herbivore_data) #variance is different for two elevations.


## ## Model 
mod_Elev700m_herbivore_abundance <- lme(Abundance.sum ~ Treatments, random= ~1|Blocks, data = Elev700m_herbivore_data,
                                  weights = varIdent(form = ~1|Treatments))


summary(mod_Elev700m_herbivore_abundance) 
anova(mod_Elev700m_herbivore_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev700m_herbivore_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev700m_herbivore_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.Elev700m_herbivore_abundance = emmeans(mod_Elev700m_herbivore_abundance, specs = ~ Treatments, adjust="BH")

#Exlicit contrasts
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list_3 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_emm.Elev700m_herbivore_abundance <- contrast(emm.Elev700m_herbivore_abundance, method = contrast_list_3)
post_emm.Elev700m_herbivore_abundance

#Errorbar plot
g3 <- ggplot(Elev700m_herbivore_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.4) +
  labs(x="") + labs (y="Herbivore abundance") + ggtitle("700m") + coord_cartesian(ylim = c(0,1600)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(500,800), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); g3

#---------------------------------------------------
#Herbivore abundance at 1700m
Elev1700m_herbivore_data  <- insect_data_herbivore  %>%
  filter(Elev %in% "1700m") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Bartlett test for equality of variances
bartlett.test(Abundance.sum ~ Treatments, data = Elev1700m_herbivore_data) #variance is different for two elevations.


## ## Model 
mod_Elev1700m_herbivore_abundance <- lme(Abundance.sum ~ Treatments, random= ~1|Blocks, data= Elev1700m_herbivore_data,
                                     weights = varIdent(form = ~ 1|Treatments))


summary(mod_Elev1700m_herbivore_abundance) 
anova(mod_Elev1700m_herbivore_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev1700m_herbivore_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev1700m_herbivore_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.Elev1700m_herbivore_abundance = emmeans(mod_Elev1700m_herbivore_abundance, specs = ~ Treatments, adjust="BH")

#Explicit contrasts
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list_4 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_Elev1700m_herbivore_abundance <- contrast(emm.Elev1700m_herbivore_abundance, method = contrast_list_4)
post_Elev1700m_herbivore_abundance

#Errorbar plot
g4 <- ggplot(Elev1700m_herbivore_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.4) +
  labs(x="Treatments") + labs (y="Herbivore abundance") + ggtitle("1700m") + coord_cartesian(ylim = c(0,1600)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(550,900), label = c("**","*"),label.size = 7,
    tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); g4


#--------------------------------------------------------------------------------------------------------------
#combine plots
insect_abundance_plot <-cowplot::plot_grid(g1, g3, g2 , g4,
                   ncol = 2, byrow = TRUE,labels = c('A', 'B','C','D'), align="hv", label_size = 16); insect_abundance_plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("insect_abundance_plot.tiff", width = 20, height = 20, units = "cm", dpi = 600)


  
  


