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

#load data
combine_insects_data <- read_excel("datasets/CombineSite_Insects_Orders_data.xlsx",
                                  sheet="Guilds")


##########################################################################################################################################
#Other Insects and Arachnids (spiders) abundance for separate sites [Others]
#################################################################################################

#Other Insect abundance at 700m 
Elev700m_others_data  <- combine_insects_data  %>%
  filter(Elev %in% "700m", Guilds %in% "Others") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Bartlett test for equality of variances
bartlett.test(Abundance.sum ~ Treatments, data = Elev700m_others_data) #variance is different for treatments

## ## Model 
mod_Elev700m_others_abundance <- lme(Abundance.sum ~ Treatments, random= ~1|Blocks, data= Elev700m_others_data,
    weights = varIdent(form = ~ 1|Treatments))
#Since variances based on biomass differs among treatments, we used the expression ~ 1|Treatments as a one-sided 
#formula to show that the variance differs between the levels of Treatments.

#summary and Anova
summary(mod_Elev700m_others_abundance) 
anova(mod_Elev700m_others_abundance)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev700m_others_abundance, type = "pearson")^2/ df.residual(mod_Elev700m_others_abundance))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev700m_others_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev700m_others_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.Elev700m_others_abundance = emmeans(mod_Elev700m_others_abundance, specs = ~ Treatments)

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

post_Elev700m_others_abundance <- contrast(emm.Elev700m_others_abundance, method = contrast_list_1)
post_Elev700m_others_abundance

#Errorbar plot
g1 <- ggplot(Elev700m_others_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter(size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.5) +
  labs(x="") + labs (y="Others abundance (Insects+Arachnids)") + ggtitle("700m") + coord_cartesian(ylim = c(0,200)) +
  geom_bracket(
            xmin = c("C","C"), xmax = c("I","WI"),
            y.position = c(160,190), label = c("***","***"),label.size = 7,
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
Elev1700m_others_data  <- combine_insects_data %>%
  filter(Elev %in% "1700m", Guilds %in% "Others") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Bartlett test for equality of variances
bartlett.test(Abundance.sum ~ Treatments, data = Elev1700m_others_data) #variance is different for treatments


## ## Model 
mod_Elev1700m_others_abundance <- lme(Abundance.sum ~ Treatments, random= ~1|Blocks, data= Elev1700m_others_data,
                                     weights = varIdent(form = ~ 1|Treatments),control = lmeControl(opt = "optim"))
#Since variances based on biomass differs among treatments, we used the expression ~ 1|Treatments as a one-sided 
#formula to show that the variance differs between the levels of Treatments.
#We added the function: control = lmeControl(opt = "optim") to remove false convergence.

summary(mod_Elev1700m_others_abundance) 
anova(mod_Elev1700m_others_abundance)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev1700m_others_abundance, type = "pearson")^2/ df.residual(mod_Elev1700m_others_abundance))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev1700m_others_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev1700m_others_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.Elev1700m_others_abundance = emmeans(mod_Elev1700m_others_abundance, specs = ~ Treatments, adjust="BH")

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

post_Elev1700m_others_abundance <- contrast(emm.Elev1700m_others_abundance, method = contrast_list_2)
post_Elev1700m_others_abundance

#Errorbar plot
g2 <- ggplot(Elev1700m_others_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.5) +
  labs(x="Treatments") + labs (y="Others abundance (Insects+Arachnids)") + ggtitle("1700m") + coord_cartesian(ylim = c(0,200)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(120,160), label = c("***","**"),label.size = 7,
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

#Herbivore abundance at 700m
Elev700m_herbivore_data  <- combine_insects_data %>%
  filter(Elev %in% "700m",Guilds %in% "Herbivores") %>%
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
  stat_summary(fun.y="mean", size=0.5) +
  labs(x="") + labs (y="Herbivore abundance") + ggtitle("700m") + coord_cartesian(ylim = c(0,1510)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(450,800), label = c("***","***"),label.size = 7,
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
Elev1700m_herbivore_data  <- combine_insects_data  %>%
  filter(Elev %in% "1700m",,Guilds %in% "Herbivores") %>%
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
  stat_summary(fun.y="mean", size=0.5) +
  labs(x="Treatments") + labs (y="Herbivore abundance") + ggtitle("1700m") + coord_cartesian(ylim = c(0,1510)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(550,1000), label = c("**","*"),label.size = 7,
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
ggsave("insect_abundance_plot.tiff", width = 23, height = 23, units = "cm", dpi = 600)


  
  


