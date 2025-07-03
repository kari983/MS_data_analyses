# load the packages
library(readxl)
library(vegan)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(DHARMa)
library(glmmTMB)
library(reshape2)
library(lme4)
library(lmerTest)
library(nlme)
library(car)
library(ggpubr)

#delete all the variables that are there in the environment
#rm(list=ls()) 

##########################################################################################################################################
#Total Insect abundance for separate sites
#################################################################################################
#load data
insect_data1 <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/CombineSite_Insects_Orders_data.xlsx",
                           sheet="combine_insect_abundance")

#Numba data
numba_insect_data  <- insect_data1  %>%
  filter(Site=="Numba") %>%
  group_by(Garden, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Levene's Test for equality of variances
leveneTest(Abundance.sum ~ Treatments, data = numba_insect_data ) #variance is different for two elevations.
bartlett.test(Abundance.sum ~ Treatments, data = numba_insect_data ) #variance is different for two elevations.


## ## Model 
mod_numba_insect_abundance <- gls(Abundance.sum ~ Treatments, data = numba_insect_data ,
                    weights = varIdent(form = ~1|Treatments))


summary(mod_numba_insect_abundance) 
anova(mod_numba_insect_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_insect_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_insect_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.numba_insect_abundance = emmeans(mod_numba_insect_abundance, specs = ~ Treatments, adjust="BH")

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

post_numba_insect_abundance <- contrast(emm.numba_insect_abundance, method = contrast_list_1)
post_numba_insect_abundance

#Errorbar plot
g1 <- ggplot(numba_insect_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="Total insect abundance") + ggtitle("700m") + ylim(0,560) +
  #annotate("text", 
  #label = c("ab","a","c","bc"),
  #x = c(1,2,3,4), 
  #y = c(40,32,48,43), size = 5, colour = c("darkblue")) + 
  geom_bracket(
            xmin = c("C","C"), xmax = c("I","WI"),
            y.position = c(470,548), label = c("***","***"),label.size = 7,
            tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")); g1


#Yawan data
yawan_insect_data  <- insect_data1  %>%
  filter(Site=="Yawan") %>%
  group_by(Garden, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Levene's Test for equality of variances
leveneTest(Abundance.sum ~ Treatments, data = yawan_insect_data) #variance is different for two elevations.
bartlett.test(Abundance.sum ~ Treatments, data = yawan_insect_data) #variance is different for two elevations.


## ## Model 
mod_yawan_insect_abundance <- gls(Abundance.sum ~ Treatments, data = yawan_insect_data ,
                                  weights = varIdent(form = ~1|Treatments))


summary(mod_yawan_insect_abundance) 
anova(mod_yawan_insect_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_insect_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_insect_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.yawan_insect_abundance = emmeans(mod_yawan_insect_abundance, specs = ~ Treatments, adjust="BH")

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

post_yawan_insect_abundance <- contrast(emm.numba_insect_abundance, method = contrast_list_2)
post_yawan_insect_abundance

#Errorbar plot
g2 <- ggplot(yawan_insect_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="Total insect abundance") + ggtitle("1700m") + ylim(0,560) +
  #annotate("text", 
  #label = c("ab","a","c","bc"),
  #x = c(1,2,3,4), 
  #y = c(40,32,48,43), size = 5, colour = c("darkblue")) + 
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(380,460), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")) ;g2


#combine for plots
cowplot::plot_grid(g1 , g2,
                   ncol = 2, byrow = TRUE,labels = c('(A)', '(B)'), align="hv")


##########################################################################################################################################
#Herbivore abundance
#################################################################################################
#load data
insect_data_herbivore <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/CombineSite_Insects_Orders_data.xlsx",
                           sheet="herbivores")

#Numba data
numba_herbivore_data  <- insect_data_herbivore %>%
  filter(Site=="Numba") %>%
  group_by(Garden, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Levene's Test for equality of variances
leveneTest(Abundance.sum ~ Treatments, data = numba_herbivore_data ) #variance is different for two elevations.
bartlett.test(Abundance.sum ~ Treatments, data = numba_herbivore_data ) #variance is different for two elevations.


## ## Model 
mod_numba_numba_herbivore_abundance <- gls(Abundance.sum ~ Treatments, data = numba_herbivore_data ,
                                  weights = varIdent(form = ~1|Treatments))


summary(mod_numba_numba_herbivore_abundance ) 
anova(mod_numba_numba_herbivore_abundance )

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_numba_numba_herbivore_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_numba_numba_herbivore_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.numba_herbivore_abundance = emmeans(mod_numba_numba_herbivore_abundance, specs = ~ Treatments, adjust="BH")

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

post_numba_herbivore_abundance <- contrast(emm.numba_herbivore_abundance, method = contrast_list_1)
post_numba_herbivore_abundance 

#Errorbar plot
gg1 <- ggplot(numba_herbivore_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="Herbivore abundance") + ggtitle("700m") + ylim(0,560) +
  #annotate("text", 
  #label = c("ab","a","c","bc"),
  #x = c(1,2,3,4), 
  #y = c(40,32,48,43), size = 5, colour = c("darkblue")) + 
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(390,500), label = c("***","***"),label.size = 7,
    tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")); gg1


#Yawan data
yawan_herbivore_data  <- insect_data_herbivore  %>%
  filter(Site=="Yawan") %>%
  group_by(Garden, Treatments) %>%
  summarise(Abundance.sum = sum(Abundance))


#Levene's Test for equality of variances
leveneTest(Abundance.sum ~ Treatments, data = yawan_herbivore_data ) #variance is different for two elevations.
bartlett.test(Abundance.sum ~ Treatments, data = yawan_herbivore_data ) #variance is different for two elevations.


## ## Model 
mod_yawan_herbivore_abundance <- gls(Abundance.sum ~ Treatments, data = yawan_herbivore_data,
                                  weights = varIdent(form = ~1|Treatments))


summary(mod_yawan_herbivore_abundance) 
anova(mod_yawan_herbivore_abundance)

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_yawan_herbivore_abundance, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_yawan_herbivore_abundance, ~ resid(., type = "p") |Treatments, abline = c(0, 1)) #by Treatments

# emmeans
emm.yawan_herbivore_abundance = emmeans(mod_yawan_herbivore_abundance, specs = ~ Treatments, adjust="BH")

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

post_yawan_herbivore_abundance <- contrast(emm.yawan_herbivore_abundance, method = contrast_list_2)
post_yawan_herbivore_abundance

#Errorbar plot
gg2 <- ggplot(yawan_insect_data) +
  aes(x = Treatments, y = Abundance.sum) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="Herbivore abundance") + ggtitle("1700m") + ylim(0,560) +
  #annotate("text", 
  #label = c("ab","a","c","bc"),
  #x = c(1,2,3,4), 
  #y = c(40,32,48,43), size = 5, colour = c("darkblue")) + 
  geom_bracket(
    xmin = c("C","C"), xmax = c("I","WI"),
    y.position = c(400,490), label = c("**","*"),label.size = 7,
    tip.length = 0.0, color="blue") + 
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, angle = 0, hjust = 1, vjust = 0.35, face = "bold")) +
  theme(axis.title.x =element_text(size=14, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=15, margin = margin(0,8), face="bold")); gg2


#--------------------------------------------------------------------------------------------------------------
#combine for plots
#cowplot::plot_grid(gg1 , gg2,
                   #ncol = 2, byrow = TRUE,labels = c('A', 'B'), align="hv")

#ALL combine
insect_abundance_plot <-cowplot::plot_grid(g1, gg1, g2 , gg2,
                   ncol = 2, byrow = TRUE,labels = c('A', 'B','C','D'), align="hv", label_size = 16); insect_abundance_plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("insect_abundance_plot.tiff", width = 20, height = 24, units = "cm", dpi = 600)


  
  


