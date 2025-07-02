# (H1): Insects are accelerating succession by suppressing grasses and herbs, 
# and allow woody plants to establish.

# load the packages
library(readxl)
library(vegan)
library(ggplot2)
library(nlme)
library(emmeans)
library(multcomp)
library(reshape2)
library(dplyr)

#Setting working directory
#setwd("C:/Users/Kari Iamba/Documents/working/directory")

#delete all the variables that are there in the environment
#rm(list=ls()) 


#############################################################################################################
#load the data for Numba (700m)
H1_Plot_Woody <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                            sheet="Numba_biomass_2023")

####################################################################################################################
#Numba Woody biomass LRR
####################################################################################################################

#(A) Numba Woody data (700m)
Plot_Woody1 <- H1_Plot_Woody %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

Plot_Woody2 <- Plot_Woody1  %>%
  dcast(Gardens ~ Treatments, value.var='Biomass') 


#Log Response Ratios
Plot_Woody2$Insects <- log(Plot_Woody2$W/Plot_Woody2$WI) #insect effect
Plot_Woody2$NWP     <- log(Plot_Woody2$I/Plot_Woody2$WI) #NWP effect
Plot_Woody2$`NWP+Insects`    <- log(Plot_Woody2$C/Plot_Woody2$WI) #combine effect

Plot_Woody3 <- Plot_Woody2 %>% 
  melt(id.vars = c("Gardens"),variable.name = "Treatments",
       value.name = "Biomass")

Plot_Woody4 <- Plot_Woody3 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#Plot_Woody4 <- Plot_Woody3[37:63,]

# Model 
mod_LRR1 <- lme(Biomass ~ 0 + Treatments, random = ~1|Gardens, data= Plot_Woody4) #setting reference to zero

# summary
summary(mod_LRR1)
anova(mod_LRR1)

#Check for overdispersion
dispersion <- sum(residuals(mod_LRR1, type = "pearson")^2/ df.residual(mod_LRR1))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_LRR1, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_LRR1, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Custom contrasts
emm_LRR1 = emmeans(mod_LRR1, specs = ~ Treatments)

# Mean Groupings 
#cldisplay_emm_LRR1  <- cld(emm_LRR1 , Letters = letters,  alpha = 0.05)
#cldisplay_emm_LRR1 


#Error bars with pairwise comparison (refer to model summary for significant differences)
g1 <- ggplot(Plot_Woody4) +
  aes(x = Treatments, y = Biomass) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("**"),
           x = c(3), 
           y = c(1), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [biomass]") + ggtitle("700m") + ylim(-5,4) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));  g1 



####################################################################################################################
#Yawan Woody biomass LRR
####################################################################################################################
#Load data for Yawan (1700m)
yawan_biomass_data  <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/google drive_backup/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                          sheet = "Yawan_biomass_2023")

#(B) Woody data for Yawan (1700m)
yawan_woody_biomass <- yawan_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

yawan_woody_biomass2 <- yawan_woody_biomass   %>%
  dcast(Gardens ~ Treatments, value.var='Biomass') 


#Log Response Ratios
yawan_woody_biomass2$Insects <- log(yawan_woody_biomass2$W/yawan_woody_biomass2$WI) #insect effect
yawan_woody_biomass2$NWP     <- log(yawan_woody_biomass2$I/yawan_woody_biomass2$WI) #NWP effect
yawan_woody_biomass2$`NWP+Insects`    <- log(yawan_woody_biomass2$C/yawan_woody_biomass2$WI) #combine effect

yawan_woody_biomass3 <- yawan_woody_biomass2  %>% 
  melt(id.vars = c("Gardens"),variable.name = "Treatments",
       value.name = "Biomass")

yawan_woody_biomass4 <- yawan_woody_biomass3 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#yawan_woody_biomass4 <- yawan_woody_biomass3[37:63,]

# Model 
mod_LRR2 <- lme(Biomass ~ 0 + Treatments, random = ~1|Gardens, data= yawan_woody_biomass4) #setting reference to zero

# summary
summary(mod_LRR2)
anova(mod_LRR2)

#Check for overdispersion
dispersion <- sum(residuals(mod_LRR1, type = "pearson")^2/ df.residual(mod_LRR1))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_LRR2, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_LRR2, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Custom contrasts
emm_LRR2 = emmeans(mod_LRR2, specs = ~ Treatments)

# Mean Groupings 
#cldisplay_emm_LRR2  <- cld(emm_LRR2 , Letters = letters,  alpha = 0.05)
#cldisplay_emm_LRR2 


#Error bars with pairwise comparison (refer to model summary for significant differences)
g2 <- ggplot(yawan_woody_biomass4) +
  aes(x = Treatments, y = Biomass) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("***","***"),
           x = c(2,3), 
           y = c(0.4,0.4), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [biomass]") + ggtitle("1700m") +  ylim(-5,4) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); g2  


#combine for biomass
cowplot::plot_grid(g1 , g2,
                   ncol = 2, byrow = TRUE,labels = c('A', 'B'), align="hv")


####################################################################################################################
#Numba Woody diversity LRR
####################################################################################################################
#Date for Numba (700m)
numba_biomass_data <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/google drive_backup/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                                 sheet = "Numba_biomass_2023")

#Woody plant biomass in Numba (700m)
numba_biomass_richness   <-   numba_biomass_data   %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

numba_biomass_richness2  <- numba_biomass_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

numba_biomass_richness2 [is.na(numba_biomass_richness2)] <- 0 #removing NAs
numba_biomass_richness2 

#Richness
numba_biomass_richness2 $Richness <- specnumber(numba_biomass_richness2[,3:117]) #Number of plant species 
numba_biomass_richness2 

#Shannon diversity index
numba_biomass_richness2$Shannon <- diversity(numba_biomass_richness2[,3:117], index="shannon")
numba_biomass_richness2 

#dcast based on Shannon index
numba_biomass_richness3  <- numba_biomass_richness2 %>% 
  reshape2::dcast(Gardens ~ Treatments, value.var = "Shannon")

#Log Response Ratios
numba_biomass_richness3$Insects <- log(numba_biomass_richness3$W/numba_biomass_richness3$WI) #insect effect
numba_biomass_richness3$NWP     <- log(numba_biomass_richness3$I/numba_biomass_richness3$WI) #NWP effect
numba_biomass_richness3$`NWP+Insects`    <- log(numba_biomass_richness3$C/numba_biomass_richness3$WI) #combine effect

numba_biomass_richness4  <- numba_biomass_richness3   %>% 
  melt(id.vars = c("Gardens"),variable.name = "Treatments",
       value.name = "Shannon")

numba_biomass_richness5 <- numba_biomass_richness4[-47,] #remove infinite value

numba_biomass_richness6 <- numba_biomass_richness5 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#numba_biomass_richness6 <- numba_biomass_richness5[c(37:62),]


# Model 
mod_LRR3 <- lme(Shannon ~ 0 + Treatments, random = ~1|Gardens, data= numba_biomass_richness6) #setting reference to zero

# summary
summary(mod_LRR3)
anova(mod_LRR3)

#Check for overdispersion
dispersion <- sum(residuals(mod_LRR3, type = "pearson")^2/ df.residual(mod_LRR3))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_LRR3, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_LRR3, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Custom contrasts
emm_LRR3 = emmeans(mod_LRR3, specs = ~ Treatments)

# Mean Groupings 
#cldisplay_emm_LRR3  <- cld(emm_LRR3 , Letters = letters,  alpha = 0.05)
#cldisplay_emm_LRR3 


#Error bars with pairwise comparison (refer to model summary for significant differences)
g3 <- ggplot(numba_biomass_richness6) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("***","**"),
           x = c(2,3), 
           y = c(-0.3,0.1), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [diversity]") + ggtitle("700m") +  ylim(-3,0.7) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ; g3


####################################################################################################################
#Yawan Woody diversity LRR
####################################################################################################################
#Data for Yawan (1700m)
yawan_biomass_data  <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/google drive_backup/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                                  sheet = "Yawan_biomass_2023")


#Woody biomass in Yawan (1700m)
yawan_biomass_richness   <-   yawan_biomass_data   %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

yawan_biomass_richness2  <- yawan_biomass_richness %>% 
  reshape2::dcast(Gardens +  Treatments  ~ Plant_sp, value.var = "Biomass")

yawan_biomass_richness2[is.na(yawan_biomass_richness2)] <- 0 #removing NAs
yawan_biomass_richness2

#Richness
yawan_biomass_richness2$Richness <- specnumber(yawan_biomass_richness2[,3:105]) #Number of plant species 
yawan_biomass_richness2

#Shannon diversity index
yawan_biomass_richness2$Shannon <- diversity(yawan_biomass_richness2[,3:105], index="shannon")
yawan_biomass_richness2

#dcast based on Shannon index
yawan_biomass_richness3  <- yawan_biomass_richness2 %>% 
  reshape2::dcast(Gardens ~ Treatments, value.var = "Shannon")

#Log Response Ratios
yawan_biomass_richness3$Insects <- log(yawan_biomass_richness3$W/yawan_biomass_richness3$WI) #insect effect
yawan_biomass_richness3$NWP     <- log(yawan_biomass_richness3$I/yawan_biomass_richness3$WI) #NWP effect
yawan_biomass_richness3$`NWP+Insects`    <- log(yawan_biomass_richness3$C/yawan_biomass_richness3$WI) #combine effect

yawan_biomass_richness4  <- yawan_biomass_richness3  %>% 
  melt(id.vars = c("Gardens"),variable.name = "Treatments",
       value.name = "Shannon")

yawan_biomass_richness5 <- yawan_biomass_richness4 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#yawan_biomass_richness5 <- yawan_biomass_richness4[37:63,]

# Model 
mod_LRR4 <- lme(Shannon ~ 0 + Treatments, random = ~1|Gardens, data= yawan_biomass_richness5) #setting reference to zero

# summary
summary(mod_LRR4)
anova(mod_LRR4)

#Testing model
qqnorm(mod_LRR4, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_LRR4, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

# Custom contrasts
emm_LRR4 = emmeans(mod_LRR4, specs = ~ Treatments)

# Mean Groupings 
#cldisplay_emm_LRR4  <- cld(emm_LRR4 , Letters = letters,  alpha = 0.05)
#cldisplay_emm_LRR4 


#Plot with pairwise comparison
g4 <- ggplot(yawan_biomass_richness5) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("***","**"),
           x = c(2,3), 
           y = c(-0.2,0.4), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [diversity]") + ggtitle("1700m") +   ylim(-3,0.7) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); g4 


#----------------------------------------------------------------------------------------------------
#All plots combine
#LRR_WP_biomass_plot <- cowplot::plot_grid(g1, g2, g3 , g4,
                   #ncol = 2, byrow = TRUE,labels = c('A', 'B','C','D'), align="hv"); LRR_WP_biomass_plot

LRR_WP_biomass_plot <- ggarrange(g1, g3, g2 , g4, ncol = 2, nrow = 2,
          labels = c('A', 'B','C','D')); LRR_WP_biomass_plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("LRR_WP_biomass_plot.tiff", width = 20, height = 20, units = "cm", dpi = 600)

