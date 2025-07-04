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

#Clear the environment
#rm(list=ls()) 


######################################################################################################
#Biomass data for Numba (700m) and Yawan (1700m)

#load the data for Numba (700m)
numba_biomass_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                                 sheet="Numba_biomass_2023")

#load the data for Yawan (1700m)
yawan_biomass_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                                 sheet = "Yawan_biomass_2023")

##################################################################################################
##################################################################################################
#DIVERSITY IN NUMBA (700m)
########################################################################################

#(A) Total diversity in Numba (700m)
Total_numba_richness  <-  numba_biomass_data %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

Total_numba_richness2  <- Total_numba_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

Total_numba_richness2[is.na(Total_numba_richness2)] <- 0 #removing NAs and replacing with zeroes

#Richness (Number of plant species)
Total_numba_richness2$Richness <- specnumber(Total_numba_richness2[,3:198]) 
Total_numba_richness2

#Simpson diversity index
Total_numba_richness2$Simpson <- diversity(Total_numba_richness2[,3:198], index="simpson")
Total_numba_richness2 

#Shannon diversity index
Total_numba_richness2$Shannon <- diversity(Total_numba_richness2[,3:198], index="shannon")
Total_numba_richness2

#Simpson's Dominance Index is the inverse of the Simpson's Index (1/D).
dominance_values <- 1-Total_numba_richness2$Simpson
Total_numba_richness2$Dominance  <- dominance_values 
Total_numba_richness2

#Model
mod_numba_total_rich <- lme(Shannon ~ Treatments, random= ~1|Gardens, data= Total_numba_richness2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_total_rich, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_total_rich, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_numba_total_rich)
anova(mod_numba_total_rich)

#estimated means (Post Hoc)
emm.numba_total_rich <- emmeans(mod_numba_total_rich, specs = ~ Treatments )

#Explicit contrasts
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list7 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_numba_total_rich  <- contrast(emm.numba_total_rich, method = contrast_list7)
post_hoc_numba_total_rich

#Error bars with pairwise comparison
p_numba_total_rich <- ggplot(Total_numba_richness2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="Total diversity [H]") + ggtitle("700m") + ylim(0,2.3) +
  geom_bracket(
    xmin = c("C"), xmax = c("I"),
    y.position = c(2), label = c("*"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_numba_total_rich

#-------------------------------------------
#(B) NWP diversity in Numba (700m)
NWP_numba_richness  <-  numba_biomass_data %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

NWP_numba_richness2  <- NWP_numba_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

NWP_numba_richness2[is.na(NWP_numba_richness2)] <- 0 #removing NAs and replacing with zeroes

#Richness
NWP_numba_richness2$Richness <- specnumber(NWP_numba_richness2[,3:83]) #Number of plant species 
NWP_numba_richness2 

#Simpson diversity index
NWP_numba_richness2$Simpson <- diversity(NWP_numba_richness2[,3:83], index="simpson")
NWP_numba_richness2

#Shannon diversity index
NWP_numba_richness2$Shannon <- diversity(NWP_numba_richness2[,3:83], index="shannon")
NWP_numba_richness2

#Simpson's Dominance Index is the inverse of the Simpson's Index (1/D).
dominance_values <- 1-NWP_numba_richness2$Simpson
NWP_numba_richness2$Dominance  <- dominance_values 
NWP_numba_richness2

#Model
mod_NWP_numba_richness <- lme(Shannon ~ Treatments, random= ~1|Gardens, data= NWP_numba_richness2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_NWP_numba_richness, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_NWP_numba_richness, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_NWP_numba_richness)
anova(mod_NWP_numba_richness)

#estimated means (Post Hoc)
emm.NWP_numba_richness <- emmeans(mod_NWP_numba_richness, specs = ~ Treatments )

#Explicit contrasts
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list8 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_NWP_numba_richness <- contrast(emm.NWP_numba_richness, method = contrast_list8)
post_hoc_NWP_numba_richness

#Error bars with pairwise comparison
p_NWP_numba_richness <- ggplot(NWP_numba_richness2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="NWP diversity [H]") + ggtitle("700m") +  ylim(0,2.3) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(1.8,2.2), label = c("**","***"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_NWP_numba_richness


#(C) Woody richness in Numba (700m)
W_numba_richness  <-  numba_biomass_data %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

W_numba_richness2  <- W_numba_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

W_numba_richness2[is.na(W_numba_richness2)] <- 0 #removing NAs and replacing with zeroes

#Richness
W_numba_richness2$Richness <- specnumber(W_numba_richness2[,3:117]) #Number of plant species 
W_numba_richness2 

#Simpson diversity index
W_numba_richness2$Simpson <- diversity(W_numba_richness2[,3:117], index="simpson")
W_numba_richness2#adding simp as a new column name

#Shannon diversity index
W_numba_richness2$Shannon <- diversity(W_numba_richness2[,3:117], index="shannon")
W_numba_richness2

#Simpson's Dominance Index is the inverse of the Simpson's Index (1/D).
dominance_values <- 1-W_numba_richness2$Simpson
W_numba_richness2$Dominance  <- dominance_values 
W_numba_richness2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = W_numba_richness2) #variance is not different 


#Model
mod_W_numba_richness <- lme(Shannon ~ Treatments, random= ~1|Gardens, data= W_numba_richness2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_W_numba_richness, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_W_numba_richness, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_W_numba_richness)
anova(mod_W_numba_richness)

#estimated means (Post Hoc)
emm.W_numba_richness <- emmeans(mod_W_numba_richness, specs = ~ Treatments )

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list9 <- list("C - I"    = C  - I,
                       "C  - W "   = C  - W,
                       "C  - WI "  = C  - WI,
                       "I  - W "   = I  - W,
                       "I  - WI"   = I  - WI,
                       "W  - WI"   = W  - WI)

post_hoc_W_numba_richness <- contrast(emm.W_numba_richness, method = contrast_list9)
post_hoc_W_numba_richness 

#Error bars with pairwise comparison
p_W_numba_richness <- ggplot(W_numba_richness2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="WP diversity [H]") + ggtitle("700m") + ylim(0,2.3) +
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(1.5,2.2), label = c("*","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_W_numba_richness


##################################################################################################
#DIVERSITY IN YAWAN (1700m)
########################################################################################

#(A) Total diversity Yawan (1700m)
Total_yawan_richness  <-  yawan_biomass_data %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  

Total_yawan_richness2  <- Total_yawan_richness%>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

Total_yawan_richness2[is.na(Total_yawan_richness2)] <- 0 #removing NAs

#Richness
Total_yawan_richness2$Richness <- specnumber(Total_yawan_richness2[,3:190]) #Number of plant species 
Total_yawan_richness2

#Simpson diversity index
Total_yawan_richness2$Simpson <- diversity(Total_yawan_richness2[,3:190], index="simpson")
Total_yawan_richness2 

#Shannon diversity index
Total_yawan_richness2$Shannon <- diversity(Total_yawan_richness2[,3:190], index="shannon")
Total_yawan_richness2

#Simpson's Dominance Index is the inverse of the Simpson's Index (1/D).
dominance_values <- 1-Total_yawan_richness2$Simpson
Total_yawan_richness2$Dominance  <- dominance_values 
Total_yawan_richness2

#Model
mod_yawan_total_rich <- lme(Shannon ~ Treatments, random= ~1|Gardens, data= Total_yawan_richness2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_total_rich, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_total_rich, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_yawan_total_rich)
anova(mod_yawan_total_rich)

#estimated means (Post Hoc)
emm.yawan_total_rich <- emmeans(mod_yawan_total_rich, specs = ~ Treatments )

#Explicit contrasts
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list10 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_hoc_yawan_total_rich  <- contrast(emm.yawan_total_rich, method = contrast_list10)
post_hoc_yawan_total_rich 

#Error bars with pairwise comparison
p_yawan_total_rich <- ggplot(Total_yawan_richness2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="Total diversity [H]") + ggtitle("1700m") + ylim(0,2.8) +
  #annotate("text", 
  #label = c("ab","a","c","bc"),
  #x = c(1,2,3,4), 
  #y = c(40,32,48,43), size = 5, colour = c("darkblue")) + 
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(2.3,2.7), label = c("*","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_yawan_total_rich 

#-------------------------------------------
#(B) NWP diversity in Yawan (1700m)
NWP_yawan_richness  <-  yawan_biomass_data %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

NWP_yawan_richness2  <- NWP_yawan_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

NWP_yawan_richness2[is.na(NWP_yawan_richness2)] <- 0 #removing NAs

#Richness
NWP_yawan_richness2$Richness <- specnumber(NWP_yawan_richness2[,3:87]) #Number of plant species 
NWP_yawan_richness2

#Simpson diversity index
NWP_yawan_richness2$Simpson <- diversity(NWP_yawan_richness2[,3:83], index="simpson")
NWP_yawan_richness2#adding simp as a new column name

#Shannon diversity index
NWP_yawan_richness2$Shannon <- diversity(NWP_yawan_richness2[,3:83], index="shannon")
NWP_yawan_richness2

#Simpson's Dominance Index is the inverse of the Simpson's Index (1/D).
dominance_values <- 1-NWP_yawan_richness2$Simpson
NWP_yawan_richness2$Dominance  <- dominance_values 
NWP_yawan_richness2

#Model
mod_NWP_yawan_richness <- lme(Shannon ~ Treatments, random= ~1|Gardens, data= NWP_yawan_richness2) 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_NWP_yawan_richness, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_NWP_yawan_richness, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_NWP_yawan_richness)
anova(mod_NWP_yawan_richness)

#estimated means (Post Hoc)
emm.NWP_yawan_richness <- emmeans(mod_NWP_yawan_richness, specs = ~ Treatments )

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list11 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_hoc_NWP_yawan_richness <- contrast(emm.NWP_yawan_richness, method = contrast_list11)
post_hoc_NWP_yawan_richness

#Error bars with pairwise comparison
p_NWP_yawan_richness <- ggplot(NWP_yawan_richness2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="NWP diversity [H]") + ggtitle("1700m") + ylim(0,2.8) +
  geom_bracket(
        xmin = c("C","C"), xmax = c("W","WI"),
        y.position = c(2.1,2.5), label = c("***","**"),label.size = 7,
        tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_NWP_yawan_richness


#-------------------------------------------
#(C) Woody diversity in Yawan (1700m)
W_yawan_richness  <-  yawan_biomass_data %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

W_yawan_richness2  <- W_yawan_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

W_yawan_richness2[is.na(W_yawan_richness2)] <- 0 #removing NAs

#Richness
W_yawan_richness2$Richness <- specnumber(W_yawan_richness2[,3:105]) #Number of plant species 
W_yawan_richness2

#Simpson diversity index
W_yawan_richness2$Simpson <- diversity(W_yawan_richness2[,3:105], index="simpson")
W_yawan_richness2#adding simp as a new column name

#Shannon diversity index
W_yawan_richness2$Shannon <- diversity(W_yawan_richness2[,3:105], index="shannon")
W_yawan_richness2

#Simpson's Dominance Index is the inverse of the Simpson's Index (1/D).
dominance_values <- 1-W_yawan_richness2$Simpson
W_yawan_richness2$Dominance  <- dominance_values 
W_yawan_richness2

##Bartlett Test of Homogeneity of Variances among treatments
bartlett.test(Richness ~ Treatments, data = W_yawan_richness2) #variance is different for the treatments

#Model
mod_W_yawan_richness <- gls(Shannon ~ Treatments, data= W_yawan_richness2)
#Since there is heterogeneity of variances (not similar) among treatments, we used the sister model to lme "gls". 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_W_yawan_richness, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_W_yawan_richness, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Summary and Anova
summary(mod_W_yawan_richness)
anova(mod_W_yawan_richness)

#estimated means (Post Hoc)
emm.W_yawan_richness <- emmeans(mod_W_yawan_richness, specs = ~ Treatments )

#Explicit contrast
C    <-  c(1,0,0,0)
I    <-  c(0,1,0,0)
W    <-  c(0,0,1,0)
WI   <-  c(0,0,0,1)

contrast_list12 <- list("C - I"    = C  - I,
                        "C  - W "   = C  - W,
                        "C  - WI "  = C  - WI,
                        "I  - W "   = I  - W,
                        "I  - WI"   = I  - WI,
                        "W  - WI"   = W  - WI)

post_hoc_W_yawan_richness <- contrast(emm.W_yawan_richness, method = contrast_list12)
post_hoc_W_yawan_richness

#Error bars with pairwise comparison
p_W_yawan_richness <- ggplot(W_yawan_richness2) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="Treatments") + labs (y="WP diversity [H]") + ggtitle("1700m") + ylim(0,2.8) +
  #annotate("text", 
  #label = c("ab","a","c","bc"),
  #x = c(1,2,3,4), 
  #y = c(40,32,48,43), size = 5, colour = c("darkblue")) + 
  geom_bracket(
    xmin = c("C","C"), xmax = c("W","WI"),
    y.position = c(2.2,2.6), label = c("**","**"),label.size = 7,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));p_W_yawan_richness


#-----------------------------------------------------------------------------------------------------
#ALL combine plots for diversity
Diversity_NWP_WP_plot <- cowplot::plot_grid(p_NWP_numba_richness, 
                   p_W_numba_richness,
                   p_numba_total_rich,
                   p_NWP_yawan_richness, 
                   p_W_yawan_richness,
                   p_yawan_total_rich,
                   ncol = 3, byrow = TRUE,labels = c('A', 'B','C','D','E','F'), align="hv") ; Diversity_NWP_WP_plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("Diversity_NWP_WP_plot.tiff", width = 20, height = 22, units = "cm", dpi = 600)







