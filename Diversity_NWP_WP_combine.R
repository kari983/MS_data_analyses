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

#Clear the environment
#rm(list=ls()) 

#Load combined site data
combine_elev.biomass_data <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/Manuscript/Finalized version/Final_MS_R_codes/data/Combine_Sites_Biomass_2023.xlsx",
                                        sheet="combine.site.biomass")

##################################################################################################
##################################################################################################
#Diversity at 700m
########################################################################################

#(A) Total diversity at 700m
Elev.700m_total_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m") %>%
  group_by(Blocks, Treatments,Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))

Elev.700m_total_div2  <- Elev.700m_total_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.700m_total_div2[is.na(Elev.700m_total_div2)] <- 0 #removing NAs and replacing with zeroes

#Shannon diversity index
Elev.700m_total_div2$Shannon <- diversity(Elev.700m_total_div2[,3:197], index="shannon")
Elev.700m_total_div2

#Richness
Elev.700m_total_div2$Richness <- specnumber(Elev.700m_total_div2[,3:197])
Elev.700m_total_div2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = Elev.700m_total_div2) #variance is similar for the treatments

#Model
mod_Elev.700m_total_div <- lme(Shannon ~ Treatments, random= ~1|Blocks, data= Elev.700m_total_div2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_total_div, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_total_div, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.700m_total_div)
anova(mod_Elev.700m_total_div)

#estimated means (Post Hoc)
emm.Elev.700m_total_div <- emmeans(mod_Elev.700m_total_div, specs = ~ Treatments )

#Explicit contrasts
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

post_hoc_Elev.700m_total_div  <- contrast(emm.Elev.700m_total_div, method = contrast_list1)
post_hoc_Elev.700m_total_div

#Error bars with pairwise comparison
g1 <- ggplot(Elev.700m_total_div2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="Total diversity [H]") + ggtitle("700m") + coord_cartesian(ylim = c(0, 2.3)) +
  geom_bracket(
    xmin = c("C"), xmax = c("I"),
    y.position = c(2), label = c("*"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold",size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));g1

#-------------------------------------------
#(B) NWP diversity at 700m
Elev.700m_NWP_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m", Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments,Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))

Elev.700m_NWP_div2  <- Elev.700m_NWP_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.700m_NWP_div2[is.na(Elev.700m_NWP_div2)] <- 0 #removing NAs and replacing with zeroes

#Shannon diversity index
Elev.700m_NWP_div2$Shannon <- diversity(Elev.700m_NWP_div2[,3:83], index="shannon")
Elev.700m_NWP_div2

#Richness
Elev.700m_NWP_div2$Richness <- specnumber(Elev.700m_NWP_div2[,3:83])
Elev.700m_NWP_div2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = Elev.700m_NWP_div2) #variance is similar for treatments


#Model
mod_Elev.700m_NWP_richness <- lme(Shannon ~ Treatments, random= ~1|Blocks, data= Elev.700m_NWP_div2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_NWP_richness, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_NWP_richness, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.700m_NWP_richness)
anova(mod_Elev.700m_NWP_richness)

#estimated means (Post Hoc)
emm.Elev.700m_NWP_richness <- emmeans(mod_Elev.700m_NWP_richness, specs = ~ Treatments)

#Explicit contrasts
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

post_hoc_Elev.700m_NWP_richness <- contrast(emm.Elev.700m_NWP_richness, method = contrast_list2)
post_hoc_Elev.700m_NWP_richness

#Error bars with pairwise comparison
g2 <- ggplot(Elev.700m_NWP_div2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="NWP diversity [H]") + ggtitle("700m") +  coord_cartesian(ylim = c(0, 2.3)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(1.8,2.2), label = c("**","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold",size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));g2

#-------------------------------------------
#(C) Woody richness at 700m
Elev.700m_WP_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments,Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))

Elev.700m_WP_div2  <- Elev.700m_WP_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.700m_WP_div2[is.na(Elev.700m_WP_div2)] <- 0 #removing NAs and replacing with zeroes

#Shannon diversity index
Elev.700m_WP_div2$Shannon <- diversity(Elev.700m_WP_div2[,3:116], index="shannon")
Elev.700m_WP_div2

#Richness
Elev.700m_WP_div2$Richness <- specnumber(Elev.700m_WP_div2[,3:116])
Elev.700m_WP_div2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = Elev.700m_WP_div2) #variance is similar for treatments

#Model
mod_Elev.700m_WP_richness <- lme(Shannon ~ Treatments, random= ~1|Blocks, data= Elev.700m_WP_div2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_WP_richness, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_WP_richness, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.700m_WP_richness)
anova(mod_Elev.700m_WP_richness)

#estimated means (Post Hoc)
emm.Elev.700m_WP_rich <- emmeans(mod_Elev.700m_WP_richness, specs = ~ Treatments )

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

post_hoc_Elev.700m_WP_richness <- contrast(emm.Elev.700m_WP_rich, method = contrast_list3)
post_hoc_Elev.700m_WP_richness 

#Error bars with pairwise comparison
g3 <- ggplot(Elev.700m_WP_div2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="WP diversity [H]") + ggtitle("700m") + coord_cartesian(ylim = c(0, 2.3)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(1.5,2.2), label = c("*","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold",size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));g3


##################################################################################################
#Diversity at 1700m
########################################################################################

#(A) Total diversity at 1700m
Elev.1700m_total_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m") %>%
  group_by(Blocks, Treatments,Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  

Elev.1700m_total_div2  <- Elev.1700m_total_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.1700m_total_div2[is.na(Elev.1700m_total_div2)] <- 0 #removing NAs

#Shannon diversity index
Elev.1700m_total_div2$Shannon <- diversity(Elev.1700m_total_div2[,3:190], index="shannon")
Elev.1700m_total_div2

#Richness
Elev.1700m_total_div2$Richness <- specnumber(Elev.1700m_total_div2[,3:190])
Elev.1700m_total_div2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = Elev.1700m_total_div2) #variance is similar for treatments


#Model
mod_Elev.1700m_total_div <- lme(Shannon ~ Treatments, random= ~1|Blocks, data= Elev.1700m_total_div2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_total_div, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_total_div, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.1700m_total_div)
anova(mod_Elev.1700m_total_div)

#estimated means (Post Hoc)
emm.Elev.1700m_total_div <- emmeans(mod_Elev.1700m_total_div, specs = ~ Treatments )

#Explicit contrasts
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

post_hoc_Elev.1700m_total_div  <- contrast(emm.Elev.1700m_total_div, method = contrast_list4)
post_hoc_Elev.1700m_total_div

#Error bars with pairwise comparison
g4 <- ggplot(Elev.1700m_total_div2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="Total diversity [H]") + ggtitle("1700m") + coord_cartesian(ylim = c(0, 2.8)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(2.3,2.7), label = c("*","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold",size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));g4

#-------------------------------------------
#(B) NWP diversity at 1700m
Elev.1700m_NWP_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m", Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments,Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))

Elev.1700m_NWP_div2  <- Elev.1700m_NWP_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.1700m_NWP_div2[is.na(Elev.1700m_NWP_div2)] <- 0 #removing NAs

#Shannon diversity index
Elev.1700m_NWP_div2$Shannon <- diversity(Elev.1700m_NWP_div2[,3:87], index="shannon")
Elev.1700m_NWP_div2

#Richness
Elev.1700m_NWP_div2$Richness <- specnumber(Elev.1700m_NWP_div2[,3:87])
Elev.1700m_NWP_div2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = Elev.1700m_NWP_div2) #variance is similar for the treatments


#Model
mod_Elev.1700m_NWP_div <- lme(Shannon ~ Treatments, random= ~1|Blocks, data= Elev.1700m_NWP_div2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_NWP_div, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_NWP_div, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.1700m_NWP_div)
anova(mod_Elev.1700m_NWP_div)

#estimated means (Post Hoc)
emm.Elev.1700m_NWP_div <- emmeans(mod_Elev.1700m_NWP_div, specs = ~ Treatments)

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

post_hoc_Elev.1700m_NWP_div <- contrast(emm.Elev.1700m_NWP_div, method = contrast_list5)
post_hoc_Elev.1700m_NWP_div

#Error bars with pairwise comparison
g5 <- ggplot(Elev.1700m_NWP_div2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="NWP diversity [H]") + ggtitle("1700m") + coord_cartesian(ylim = c(0, 2.8)) +
  geom_bracket(
        xmin = c("C","C"), xmax = c("W","WI"),
        y.position = c(2.1,2.5), label = c("***","**"),label.size = 7,
        tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold",size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));g5


#-------------------------------------------
#(C) Woody diversity at 1700m
Elev.1700m_WP_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments,Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

Elev.1700m_WP_div2  <- Elev.1700m_WP_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.1700m_WP_div2[is.na(Elev.1700m_WP_div2)] <- 0 #removing NAs

#Shannon diversity index
Elev.1700m_WP_div2$Shannon <- diversity(Elev.1700m_WP_div2[,3:105], index="shannon")
Elev.1700m_WP_div2

#Richness
Elev.1700m_WP_div2$Richness <- specnumber(Elev.1700m_WP_div2[,3:105])
Elev.1700m_WP_div2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = Elev.1700m_WP_div2) #variance is different for the treatments

#Model
mod_Elev.1700m_WP_div <- lme(Shannon ~ Treatments, random= ~1|Blocks, data= Elev.1700m_WP_div2,
                             weights = varIdent(form = ~ 1|Treatments))

#Since variances based on richness differs among treatments, we used the expression ~ 1|Treatments as a one-sided 
#formula to show that the variance differs between the levels of Treatments.

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_WP_div, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_WP_div, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_Elev.1700m_WP_div)
anova(mod_Elev.1700m_WP_div)

#estimated means (Post Hoc)
emm.Elev.1700m_WP_div <- emmeans(mod_Elev.1700m_WP_div, specs = ~ Treatments )

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list6 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_hoc_Elev.1700m_WP_div <- contrast(emm.Elev.1700m_WP_div, method = contrast_list6)
post_hoc_Elev.1700m_WP_div

#Error bars with pairwise comparison
g6 <- ggplot(Elev.1700m_WP_div2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="WP diversity [H]") + ggtitle("1700m") + coord_cartesian(ylim = c(0, 2.8)) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(2.2,2.6), label = c("**","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold",size=15)) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=14, margin = margin(0,8), face="bold"));g6


#-----------------------------------------------------------------------------------------------------
#ALL combine plots for diversity
Diversity_NWP_WP_plot <- cowplot::plot_grid(g2, g3, g1,
                                            g5, g6, g4,
                   ncol = 3, byrow = TRUE,labels = c('A', 'B','C','D','E','F'), align="hv") ; Diversity_NWP_WP_plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("Diversity_NWP_WP_plot.tiff", width = 25, height = 22, units = "cm", dpi = 600)







