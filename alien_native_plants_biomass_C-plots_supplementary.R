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

#clear environment
#rm(list = ls())

#Load combined site data
combine_elev.biomass_data <- read_excel("datasets/Combine_Sites_Biomass_2023.xlsx",
                                        sheet="combine.site.biomass")

##############################################################################################################
#STATUS OF WOODY BIOMASS 
########################################################################################

#(A) Status Woody biomass at 700m (for alien-native woody plants in C-plots)
Elev700m_WP_biomass_Status <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in% "woody", Treatments %in% "C")  %>%
  group_by(Blocks, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_Elev700m_WP_biomass_Status <- lme(prop_logit ~ Status, random= ~1|Blocks, 
                                 data= Elev700m_WP_biomass_Status)

# Summary and Anova
summary(mod_Elev700m_WP_biomass_Status)
anova(mod_Elev700m_WP_biomass_Status)

#estimated means (Post Hoc)
emm.Elev700m_WP_biomass_Status <- emmeans(mod_Elev700m_WP_biomass_Status, specs = ~ Status)

#Mean Groupings 
#cldisplay_numba_w.biom_Alien <- cld(emm.numba_w.biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_numba_w.biom_Alien 

#Explicit contrast
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_1 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_Elev700m_WP_biomass_Status  <- contrast(emm.Elev700m_WP_biomass_Status, method = contrast_list_1)
post_hoc_Elev700m_WP_biomass_Status

#Error bar plot with pairwise comparison
p_w.biom_700m_Alien <- ggplot(Elev700m_WP_biomass_Status) +
  aes(x = Status, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="logit (WP proportion)") + ggtitle("700m") + ylim(-5,5) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  geom_bracket(
    xmin = c(1), xmax = c(2),
    y.position = c(3), label = c("***"),label.size = 6,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_w.biom_700m_Alien


#------------------------------------------------------------------------------------
#(B) Status Woody biomass at 1700m (for alien-native woody plants in C-plots)
Elev1700m_WP_biomass_Status <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Plants %in% "woody", Treatments %in% "C")  %>%
  group_by(Blocks, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_Elev1700m_WP_biomass_Status <- lme(prop_logit ~ Status, random= ~1|Blocks, 
                              data= Elev1700m_WP_biomass_Status)

# Summary and Anova
summary(mod_Elev1700m_WP_biomass_Status)
anova(mod_Elev1700m_WP_biomass_Status)

#estimated means (Post Hoc)
emm.Elev1700m_WP_biomass_Status = emmeans(mod_Elev1700m_WP_biomass_Status, specs = ~ Status)

#Mean Groupings 
#cldisplay_yawan_w.biom_Alien <- cld(emm.yawan_w.biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_yawan_w.biom_Alien 

#Explicit contrast
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_2 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_Elev1700m_WP_biomass_Status <- contrast(emm.Elev1700m_WP_biomass_Status, method = contrast_list_2)
post_hoc_Elev1700m_WP_biomass_Status 

#Error bar plot with pairwise comparison
p_w.biom_1700m_Alien <- ggplot(Elev1700m_WP_biomass_Status) +
  aes(x = Status, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="logit (WP proportion)") + ggtitle("1700m") +  ylim(-5,5) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  geom_bracket(
    xmin = c(1), xmax = c(2),
    y.position = c(4), label = c("**"),label.size = 6,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_w.biom_1700m_Alien

#------------------------------------------------------------------------------------
#(C) Status of Non-Woody biomass at 700m (for alien-native non-woody plants fo C-plots)
Elev700m_NWP_biomass_Status <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in% "non_woody", Treatments %in% "C")  %>%
  group_by(Blocks, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_Elev700m_NWP_biomass_Status <- lme(prop_logit ~ Status, random= ~1|Blocks, 
                              data= Elev700m_NWP_biomass_Status)

# Summary and Anova
summary(mod_Elev700m_NWP_biomass_Status)
anova(mod_Elev700m_NWP_biomass_Status)

#estimated means (Post Hoc)
emm.Elev700m_NWP_biomass_Status <- emmeans(mod_Elev700m_NWP_biomass_Status, specs = ~ Status)

#Mean Groupings 
#cldisplay_numba_nw.biom_Alien <- cld(emm.numba_nw.biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_numba_nw.biom_Alien 

#Explicit
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_3 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_Elev700m_NWP_biomass_Status <- contrast(emm.Elev700m_NWP_biomass_Status, method = contrast_list_3)
post_hoc_Elev700m_NWP_biomass_Status


#Error bar plot with pairwise comparison
p_nw.biom_700m_Alien <- ggplot(Elev700m_NWP_biomass_Status) +
  aes(x = Status, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="logit (NWP proportion)") + ggtitle("700m") + ylim(-4,4) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  geom_bracket(
    xmin = c(1), xmax = c(2),
    y.position = c(3.7), label = c("***"),label.size = 6,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_nw.biom_700m_Alien 


#------------------------------------------------------------------------------------
#(D) Status Non-Woody biomass in Yawan (for alien non-woody plants in C-plots)
Elev1700m_NWP_biomass_Status <- combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Plants %in% "non_woody", Treatments %in% "C")  %>%
  group_by(Blocks, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_Elev1700m_NWP_biomass_Status <- lme(prop_logit ~ Status, random= ~1|Blocks, 
                               data= Elev1700m_NWP_biomass_Status)

#mod_yawan_nw.biom_Alien <- gls(prop_logit ~ Status, data= yawan_nw_biomass_Status,
                              #weights = varIdent(form = ~1|Status))

# Summary and Anova
summary(mod_Elev1700m_NWP_biomass_Status)
anova(mod_Elev1700m_NWP_biomass_Status)

#estimated means (Post Hoc)
emm.Elev1700m_NWP_biomass_Status <- emmeans(mod_Elev1700m_NWP_biomass_Status, specs = ~ Status)

#Mean Groupings 
#cldisplay_yawan_nw.biom_Alien <- cld(emm.Elev1700m_NWP_biomass_Status, Letters = letters,  alpha = 0.05)
#cldisplay_yawan_nw.biom_Alien 

#Explicit
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_4 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_Elev1700m_NWP_biomass_Status <- contrast(emm.Elev1700m_NWP_biomass_Status, method = contrast_list_4)
post_hoc_Elev1700m_NWP_biomass_Status

#Error bar plot with pairwise comparison
p_nw.biom_1700m_Alien <- ggplot(Elev1700m_NWP_biomass_Status) +
  aes(x = Status, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="logit (NWP proportion)") + ggtitle("1700m") + ylim(-4,4) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5, size = 17)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 13, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0.3, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_nw.biom_1700m_Alien



#---------------------------------------------------------------------------------------------------
#combine all plot
C_alien_plot <- cowplot::plot_grid(p_nw.biom_700m_Alien,
                   p_w.biom_700m_Alien,
                   p_nw.biom_1700m_Alien,
                   p_w.biom_1700m_Alien,
                   ncol = 2, byrow = TRUE,labels = c('A', 'B','C','D'), align="hv"); C_alien_plot 

#Saving ordination plot as jpg format
#ggsave("C_alien_plot.jpg", width = 20, height = 20, units = "cm")

#Saving ordination plot in tiff format (dpi = 600)
ggsave("C_alien_plot.tiff", width = 20, height = 20, units = "cm", dpi = 600)






