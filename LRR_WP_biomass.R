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

#Load combined site data
combine_elev.biomass_data <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/Manuscript/Finalized version/Final_MS_R_codes/data/Combine_Sites_Biomass_2023.xlsx",
                                        sheet="combine.site.biomass")

####################################################################################################################
#Woody biomass LRR at 700m
####################################################################################################################

#(A) Woody biomass at 700m
Elev.700m_WP_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

Elev.700m_WP_biomass2 <- Elev.700m_WP_biomass %>%
  dcast(Blocks ~ Treatments, value.var='Biomass') 


#Log Response Ratios
Elev.700m_WP_biomass2$Insects <- log(Elev.700m_WP_biomass2$W/Elev.700m_WP_biomass2$WI) #insect effect
Elev.700m_WP_biomass2$NWP     <- log(Elev.700m_WP_biomass2$I/Elev.700m_WP_biomass2$WI) #NWP effect
Elev.700m_WP_biomass2$`NWP+Insects`    <- log(Elev.700m_WP_biomass2$C/Elev.700m_WP_biomass2$WI) #combine effect

Elev.700m_WP_biomass3 <- Elev.700m_WP_biomass2 %>% 
  melt(id.vars = c("Blocks"),variable.name = "Treatments",
       value.name = "Biomass")

Elev.700m_WP_biomass4 <- Elev.700m_WP_biomass3 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#Plot_Woody4 <- Plot_Woody3[37:63,]

# Model 
mod_Elev.700m_WP_LRR <- lme(Biomass ~ 0 + Treatments, random = ~1|Blocks, data= Elev.700m_WP_biomass4) #setting reference to zero

# summary
summary(mod_Elev.700m_WP_LRR)
anova(mod_Elev.700m_WP_LRR)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.700m_WP_LRR, type = "pearson")^2/ df.residual(mod_Elev.700m_WP_LRR))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_WP_LRR, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_WP_LRR, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

#Error bars with pairwise comparison (refer to model summary for significant differences)
gr1 <- ggplot(Elev.700m_WP_biomass4) +
  aes(x = Treatments, y = Biomass) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("**"),
           x = c(3), 
           y = c(1), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [biomass]") + ggtitle("700m") + coord_cartesian(ylim = c(-5, 4)) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold"));  gr1 



####################################################################################################################
#Woody biomass LRR at 1700m
####################################################################################################################

#(B) Woody biomass of 1700m
Elev.1700m_WP_biomass <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments) %>%
  summarise(Biomass = sum(Biomass_kg))

Elev.1700m_WP_biomass2 <- Elev.1700m_WP_biomass %>%
  dcast(Blocks ~ Treatments, value.var='Biomass') 


#Log Response Ratios
Elev.1700m_WP_biomass2$Insects <- log(Elev.1700m_WP_biomass2$W/Elev.1700m_WP_biomass2$WI) #insect effect
Elev.1700m_WP_biomass2$NWP     <- log(Elev.1700m_WP_biomass2$I/Elev.1700m_WP_biomass2$WI) #NWP effect
Elev.1700m_WP_biomass2$`NWP+Insects`    <- log(Elev.1700m_WP_biomass2$C/Elev.1700m_WP_biomass2$WI) #combine effect

Elev.1700m_WP_biomass3 <- Elev.1700m_WP_biomass2 %>% 
  melt(id.vars = c("Blocks"),variable.name = "Treatments",
       value.name = "Biomass")

Elev.1700m_WP_biomass4 <- Elev.1700m_WP_biomass3 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#yawan_woody_biomass4 <- yawan_woody_biomass3[37:63,]

# Model 
mod_Elev.1700m_WP_LRR <- lme(Biomass ~ 0 + Treatments, random = ~1|Blocks, data= Elev.1700m_WP_biomass4) #setting reference to zero

# summary
summary(mod_Elev.1700m_WP_LRR)
anova(mod_Elev.1700m_WP_LRR)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.1700m_WP_LRR, type = "pearson")^2/ df.residual(mod_Elev.1700m_WP_LRR))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.1700m_WP_LRR, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_WP_LRR, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


#Error bars with pairwise comparison (refer to model summary for significant differences)
gr2 <- ggplot(Elev.1700m_WP_biomass4) +
  aes(x = Treatments, y = Biomass) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("***","***"),
           x = c(2,3), 
           y = c(0.4,0.4), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [biomass]") + ggtitle("1700m") +  coord_cartesian(ylim = c(-5, 4)) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); gr2  


#combine for biomass
cowplot::plot_grid(gr1 , gr2,
                   ncol = 2, byrow = TRUE,labels = c('A', 'B'), align="hv")


####################################################################################################################
#Woody diversity LRR at 700m
####################################################################################################################

#Woody plant diversity at 700m
Elev.700m_WP_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))


Elev.700m_WP_div2  <- Elev.700m_WP_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.700m_WP_div2[is.na(Elev.700m_WP_div2)] <- 0 #removing NAs
Elev.700m_WP_div2

#Shannon diversity index
Elev.700m_WP_div2$Shannon <- diversity(Elev.700m_WP_div2[,3:116], index="shannon")
Elev.700m_WP_div2

#Richness
Elev.700m_WP_div2$Richness <- specnumber(Elev.700m_WP_div2[,3:116]) #Number of plant species 
Elev.700m_WP_div2

#dcast based on Shannon index
Elev.700m_WP_div3  <- Elev.700m_WP_div2 %>% 
  reshape2::dcast(Blocks ~ Treatments, value.var = "Shannon")

#Log Response Ratios
Elev.700m_WP_div3$Insects <- log(Elev.700m_WP_div3$W/Elev.700m_WP_div3$WI) #insect effect
Elev.700m_WP_div3$NWP     <- log(Elev.700m_WP_div3$I/Elev.700m_WP_div3$WI) #NWP effect
Elev.700m_WP_div3$`NWP+Insects`    <- log(Elev.700m_WP_div3$C/Elev.700m_WP_div3$WI) #combine effect

Elev.700m_WP_div4  <- Elev.700m_WP_div3 %>% 
  melt(id.vars = c("Blocks"),variable.name = "Treatments",
       value.name = "Shannon")

Elev.700m_WP_div5 <- Elev.700m_WP_div4[-47,] #remove infinite value

Elev.700m_WP_div6 <- Elev.700m_WP_div5 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#numba_biomass_richness6 <- numba_biomass_richness5[c(37:62),]

# Model 
mod_Elev.700m_WP_div_LRR <- lme(Shannon ~ 0 + Treatments, random = ~1|Blocks, data= Elev.700m_WP_div6) #setting reference to zero

# summary
summary(mod_Elev.700m_WP_div_LRR)
anova(mod_Elev.700m_WP_div_LRR)

#Check for overdispersion
dispersion <- sum(residuals(mod_Elev.700m_WP_div_LRR, type = "pearson")^2/ df.residual(mod_Elev.700m_WP_div_LRR))
dispersion 

## Test model validation of normal plot of standardized residuals 
qqnorm(mod_Elev.700m_WP_div_LRR, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.700m_WP_div_LRR, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments

#Error bars with pairwise comparison (refer to model summary for significant differences)
gr3 <- ggplot(Elev.700m_WP_div6) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("***","**"),
           x = c(2,3), 
           y = c(-0.3,0.1), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [diversity]") + ggtitle("700m") +  coord_cartesian(ylim = c(-3, 0.7)) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ; gr3


####################################################################################################################
#Woody diversity LRR at 1700m
####################################################################################################################
#Woody biomass at 1700m
Elev.1700m_WP_div <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m", Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) 

Elev.1700m_WP_div2  <- Elev.1700m_WP_div %>% 
  reshape2::dcast(Blocks +  Treatments  ~ Plant_sp, value.var = "Biomass")

Elev.1700m_WP_div2[is.na(Elev.1700m_WP_div2)] <- 0 #removing NAs
Elev.1700m_WP_div2

#Shannon diversity index
Elev.1700m_WP_div2$Shannon <- diversity(Elev.1700m_WP_div2[,3:105], index="shannon")
Elev.1700m_WP_div2

#Richness
Elev.1700m_WP_div2$Richness <- specnumber(Elev.1700m_WP_div2[,3:105]) #Number of plant species 
Elev.1700m_WP_div2

#dcast based on Shannon index
Elev.1700m_WP_div3  <- Elev.1700m_WP_div2 %>% 
  reshape2::dcast(Blocks ~ Treatments, value.var = "Shannon")

#Log Response Ratios
Elev.1700m_WP_div3$Insects <- log(Elev.1700m_WP_div3$W/Elev.1700m_WP_div3$WI) #insect effect
Elev.1700m_WP_div3$NWP     <- log(Elev.1700m_WP_div3$I/Elev.1700m_WP_div3$WI) #NWP effect
Elev.1700m_WP_div3$`NWP+Insects`    <- log(Elev.1700m_WP_div3$C/Elev.1700m_WP_div3$WI) #combine effect

Elev.1700m_WP_div4  <- Elev.1700m_WP_div3  %>% 
  melt(id.vars = c("Blocks"),variable.name = "Treatments",
       value.name = "Shannon")

Elev.1700m_WP_div5 <- Elev.1700m_WP_div4 %>% filter(Treatments %in% c("Insects","NWP","NWP+Insects"))

#yawan_biomass_richness5 <- yawan_biomass_richness4[37:63,]

# Model 
mod_Elev.1700m_WP_div_LRR <- lme(Shannon ~ 0 + Treatments, random = ~1|Blocks, data= Elev.1700m_WP_div5) #setting reference to zero

# summary
summary(mod_Elev.1700m_WP_div_LRR)
anova(mod_Elev.1700m_WP_div_LRR)

#Testing model
qqnorm(mod_Elev.1700m_WP_div_LRR, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(mod_Elev.1700m_WP_div_LRR, ~ resid(., type = "p") | Treatments, abline = c(0, 1)) #by Treatments


#Plot with pairwise comparison
gr4 <- ggplot(Elev.1700m_WP_div5) +
  aes(x = Treatments, y = Shannon) +
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  geom_hline(yintercept = 0, linetype=8, color= "darkgrey") + 
  annotate("text", 
           label = c("***","**"),
           x = c(2,3), 
           y = c(-0.2,0.4), size = 7, colour = "blue") +
  labs(x="") + labs (y="LRR [diversity]") + ggtitle("1700m") + coord_cartesian(ylim = c(-3, 0.7)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")); gr4 


#----------------------------------------------------------------------------------------------------
#All plots combine
LRR_WP_combine_plot <- ggarrange(gr1, gr3, gr2 , gr4, ncol = 2, nrow = 2,
          labels = c('A', 'B','C','D')); LRR_WP_combine_plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("LRR_WP_combine_plot.tiff", width = 20, height = 20, units = "cm", dpi = 600)

