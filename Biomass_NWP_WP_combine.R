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
#rm(list=ls()) 

#Load combined site data
combine_elev.biomass_data <- read_excel("datasets/Combine_Sites_Biomass_2023.xlsx",
                                 sheet="combine.site.biomass")

##############################################################################################################
#COMBINED BIOMASS
########################################################################################
#Combined biomass for NWP and WP at 700m

#(A) Combined total biomass at 700m
Elev.700m_total_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_Elev.700m_total_biomass <- lme(log(Biomass) ~ Treatments, random= ~1| Blocks, 
                               data= Elev.700m_total_biomass) 

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.700m_total_biomass, type = "pearson")^2/ df.residual(mod_Elev.700m_total_biomass))
dispersion 


## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_total_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_total_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.700m_total_biomass)
anova(mod_Elev.700m_total_biomass, type="marginal")

#estimated means (Post Hoc)
emm.Elev.700m_total_biom <- emmeans(mod_Elev.700m_total_biomass, specs = ~ Treatments)

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

post_hoc_test1 <- contrast(emm.Elev.700m_total_biom, method = contrast_list1)
post_hoc_test1

#Errorbars with pairwise comparison
p1 <- ggplot(Elev.700m_total_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="ln (Total Biomass in kg)") + ggtitle("700m") + coord_cartesian(ylim = c(-2, 6)) +
  geom_bracket(
    xmin = c("C"), xmax = c("I"),
    y.position = c(5.5), label = c("*"),label.size = 7,
    tip.length = 0.0,colour = "blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold", size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));p1

##############################################################################################################
#(B) Non-woody Biomass at 700m

#Non-Woody data
Elev.700m_NWP_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m", Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))


#Model
mod_Elev.700m_NWP_biomass <- lme(log(Biomass) ~ Treatments, random= ~1| Blocks, data= Elev.700m_NWP_biomass) 

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.700m_NWP_biomass, type = "pearson")^2/ df.residual(mod_Elev.700m_NWP_biomass))
dispersion 


## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_NWP_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_NWP_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_Elev.700m_NWP_biomass)
anova(mod_Elev.700m_NWP_biomass)

#estimated means (Post Hoc)
emm.Elev.700m_NWP_biom <- emmeans(mod_Elev.700m_NWP_biomass, specs = ~ Treatments)

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

post_hoc_test2 <- contrast(emm.Elev.700m_NWP_biom, method = contrast_list2)
post_hoc_test2


#Errorbars with pairwise comparison
p2 <- ggplot(Elev.700m_NWP_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="ln (NWP Biomass in kg)") + ggtitle("700m") + coord_cartesian(ylim = c(-2, 6)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","W"),
    y.position = c(4.7,5.8), label = c("*","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold", size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));p2


##########################################################################
#(C) Woody Biomass at 700m
Elev.700m_WP_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in%"woody") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_Elev.700m_WP_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Blocks, 
                           data= Elev.700m_WP_biomass)  

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.700m_WP_biomass, type = "pearson")^2/ df.residual(mod_Elev.700m_WP_biomass))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_WP_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_WP_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_Elev.700m_WP_biomass)
anova(mod_Elev.700m_WP_biomass)

#estimated means (Post Hoc)
emm.Elev.700m_WP_biom <- emmeans(mod_Elev.700m_WP_biomass, specs = ~ Treatments)

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

post_hoc_test3 <- contrast(emm.Elev.700m_WP_biom, method = contrast_list3)
post_hoc_test3

#Errorbars with pairwise comparison
p3 <- ggplot(Elev.700m_WP_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="ln (WP Biomass in kg)") + ggtitle("700m") + coord_cartesian(ylim = c(-2, 6)) +
  geom_bracket(
    xmin = c("C","C","C"), xmax = c("I","W","WI"),
    y.position = c(3.3,4.5,5.6), label = c("**","**","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold", size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));p3

#########################################################################################################################
#############################################################################################################
#Combined biomass for NWP and WP at 1700m

#(A) Combined biomass at 1700m
Elev.1700m_total_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_Elev.1700m_total_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Blocks, 
                               data= Elev.1700m_total_biomass) 

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.1700m_total_biomass, type = "pearson")^2/ df.residual(mod_Elev.1700m_total_biomass))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_total_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_total_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.1700m_total_biomass)
anova(mod_Elev.1700m_total_biomass)

#estimated means (Post Hoc)
emm.Elev.1700m_total_biom <- emmeans(mod_Elev.1700m_total_biomass, specs = ~ Treatments)

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

post_hoc_test2 <- contrast(emm.Elev.1700m_total_biom, method = contrast_list2)
post_hoc_test2

#Errorbars with pairwise comparison
p4 <- ggplot(Elev.1700m_total_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.05) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="ln (Total Biomass in kg)") + ggtitle("1700m") + coord_cartesian(ylim = c(-4, 7)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold", size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));p4


#############################################################################################################
# Non-woody Biomass at 1700m

#(B) Non-Woody biomass at 1700m
Elev.1700m_NWP_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m", Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_Elev.1700m_NWP_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Blocks, 
                            data= Elev.1700m_NWP_biomass)  

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.1700m_NWP_biomass, type = "pearson")^2/ df.residual(mod_Elev.1700m_NWP_biomass))
dispersion 


## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_NWP_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_NWP_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_Elev.1700m_NWP_biomass)
anova(mod_Elev.1700m_NWP_biomass)

#estimated means (Post Hoc)
emm.Elev.1700m_NWP_biom  <- emmeans(mod_Elev.1700m_NWP_biomass, specs = ~ Treatments)

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

post_hoc_test5 <- contrast(emm.Elev.1700m_NWP_biom, method = contrast_list5)
post_hoc_test5


#Errorbars with pairwise comparison
p5 <- ggplot(Elev.1700m_NWP_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="ln (NWP Biomass in kg)") + ggtitle("1700m") +  coord_cartesian(ylim = c(-4, 7)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(5.1,6.5), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold", size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));p5



#(C)  Woody Biomass at 1700m
Elev.1700m_WP_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

#Model
mod_Elev.1700m_WP_biomass <- lme(log(Biomass) ~ Treatments , random= ~1| Blocks, 
                           data= Elev.1700m_WP_biomass)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.1700m_WP_biomass, type = "pearson")^2/ df.residual(mod_Elev.1700m_WP_biomass))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_WP_biomass, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_WP_biomass, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


# Summary and Anova
summary(mod_Elev.1700m_WP_biomass)
anova(mod_Elev.1700m_WP_biomass)

#estimated means (Post Hoc)
emm.Elev.1700m_WP_biom <- emmeans(mod_Elev.1700m_WP_biomass, specs = ~ Treatments)

#Explicit contrast
contrast_list6 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_test6 <- contrast(emm.Elev.1700m_WP_biom, method = contrast_list6)
post_hoc_test6

#Errorbars with pairwise comparison
p6 <- ggplot(Elev.1700m_WP_biomass) +
  aes(x = Treatments, y = log(Biomass)) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="ln (WP Biomass in kg)") + ggtitle("1700m") + coord_cartesian(ylim = c(-4, 7)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(5.2,6.7), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold", size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));p6


#--------------------------------------------------------------------------------------
#combine plots for biomass
biomass_NWP_WP_plot <- cowplot::plot_grid(p2, p3, p1, 
                                           p5, p6, p4,
                                          ncol = 3, byrow = TRUE,labels = c('A', 'B','C','D','E','F'), align="hv"); biomass_NWP_WP_plot 

#Saving plot as jpg format
#ggsave("biomass_NWP_WP_plot.jpg", width = 25, height = 25, units = "cm")

#Saving ordination plot in tiff format (dpi = 600)
ggsave("biomass_NWP_WP_plot.tiff", width = 25, height = 23, units = "cm", dpi = 600)






 





