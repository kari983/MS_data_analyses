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

#clear environment
rm(list = ls())

##############################################################################################################
#STATUS OF WOODY BIOMASS 
########################################################################################
# load the data for Numba (700m) and Yawan (1700m)
numba_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                         sheet="Numba_biomass_2023")

yawan_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                         sheet = "Yawan_biomass_2023")

#------------------------------------------------------------------------------------
#(A) Status Woody biomass in Numba (for alien-native woody plants in C-plots)
numba_w_biomass_Status <- numba_data %>%
  filter(Plants=="woody" & Treatments=="C")  %>%
  group_by(Gardens, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = prop * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_numba_w.biom_Alien <- lme(prop_logit ~ Status, random= ~1| Gardens, 
                                 data= numba_w_biomass_Status)

# Summary and Anova
summary(mod_numba_w.biom_Alien)
anova(mod_numba_w.biom_Alien)

#estimated means (Post Hoc)
emm.numba_w.biom_Alien <- emmeans(mod_numba_w.biom_Alien, specs = ~ Status)

#Mean Groupings 
#cldisplay_numba_w.biom_Alien <- cld(emm.numba_w.biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_numba_w.biom_Alien 

#Explicit contrast
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_1 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_numba_w.biom_Alien <- contrast(emm.numba_w.biom_Alien, method = contrast_list_1)
post_hoc_numba_w.biom_Alien

#Error bar plot with pairwise comparison
p_w.biom_numba_Alien <- ggplot(numba_w_biomass_Status) +
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
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_w.biom_numba_Alien 


#------------------------------------------------------------------------------------
#(B) Status Woody biomass in Yawan (for alien-native woody plants in C-plots)
yawan_w_biomass_Status <- yawan_data %>%
  filter(Plants=="woody" & Treatments=="C")  %>%
  group_by(Gardens, Treatments, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = Biomass/sum(Biomass) * 100)  %>% 
         as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_yawan_w.biom_Alien <- lme(prop_logit ~ Status, random= ~1| Gardens, 
                              data= yawan_w_biomass_Status)

# Summary and Anova
summary(mod_yawan_w.biom_Alien)
anova(mod_yawan_w.biom_Alien)

#estimated means (Post Hoc)
emm.yawan_w.biom_Alien = emmeans(mod_yawan_w.biom_Alien, specs = ~ Status)

#Mean Groupings 
#cldisplay_yawan_w.biom_Alien <- cld(emm.yawan_w.biom_Alien, Letters = letters,  alpha = 0.05)
#cldisplay_yawan_w.biom_Alien 

#Explicit contrast
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_2 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_yawan_w.biom_Alien <- contrast(emm.yawan_w.biom_Alien, method = contrast_list_2)
post_hoc_yawan_w.biom_Alien

#Error bar plot with pairwise comparison
p_w.biom_yawan_Alien <- ggplot(yawan_w_biomass_Status) +
  aes(x = Status, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="") + ggtitle("1700m") +  ylim(-5,5) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  geom_bracket(
    xmin = c(1), xmax = c(2),
    y.position = c(4), label = c("**"),label.size = 6,
    tip.length = 0.0, color="blue") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_w.biom_yawan_Alien


#------------------------------------------------------------------------------------
#(C) Status Non-Woody biomass in Numba (for alien-native non-woody plants fo C-plots)
numba_nw_biomass_Status <- numba_data %>%
  filter(Plants=="non_woody" & Treatments=="C")  %>%
  group_by(Gardens, Treatments, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = Biomass/sum(Biomass) * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_numba_nw.biom_Alien <- lme(prop_logit ~ Status, random= ~1| Gardens, 
                              data= numba_nw_biomass_Status)

# Summary and Anova
summary(mod_numba_nw.biom_Alien)
anova(mod_numba_nw.biom_Alien)

#estimated means (Post Hoc)
emm.numba_nw.biom_Alien <- emmeans(mod_numba_nw.biom_Alien, specs = ~ Status)

#Mean Groupings 
cldisplay_numba_nw.biom_Alien <- cld(emm.numba_nw.biom_Alien, Letters = letters,  alpha = 0.05)
cldisplay_numba_nw.biom_Alien 

#Explicit
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_3 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_numba_nw.biom_Alien <- contrast(emm.numba_nw.biom_Alien, method = contrast_list_3)
post_hoc_numba_nw.biom_Alien

plot(post_hoc_numba_nw.biom_Alien) 


#Error bar plot with pairwise comparison
p_nw.biom_numba_Alien <- ggplot(numba_nw_biomass_Status) +
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
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_nw.biom_numba_Alien 


#------------------------------------------------------------------------------------
#(D) Status Non-Woody biomass in Yawan (for alien non-woody plants in C-plots)
yawan_nw_biomass_Status <- yawan_data %>%
  filter(Plants=="non_woody" & Treatments=="C")  %>%
  group_by(Gardens, Treatments, Status) %>% 
  summarise(Biomass = sum(Biomass_kg))  %>% 
  mutate(prop = Biomass/sum(Biomass),
         prop_logit = logit(prop),  #prop_logit = log(prop/(1-prop))
         perc = Biomass/sum(Biomass) * 100)  %>% 
  as.data.frame()

#hist(numba_w_biomass_Status$prop_logit)

#Model for Alien WP
mod_yawan_nw.biom_Alien <- lme(prop_logit ~ Status, random= ~1| Gardens, 
                               data= yawan_nw_biomass_Status)

#mod_yawan_nw.biom_Alien <- gls(prop_logit ~ Status, data= yawan_nw_biomass_Status,
                              #weights = varIdent(form = ~1|Status))

# Summary and Anova
summary(mod_yawan_nw.biom_Alien)
anova(mod_yawan_nw.biom_Alien)

#estimated means (Post Hoc)
emm.yawan_nw.biom_Alien <- emmeans(mod_yawan_nw.biom_Alien, specs = ~ Status)

#Mean Groupings 
cldisplay_yawan_nw.biom_Alien <- cld(emm.yawan_nw.biom_Alien, Letters = letters,  alpha = 0.05)
cldisplay_yawan_nw.biom_Alien 

#Explicit
Native    <-   c(1,0)
Alien     <-   c(0,1)

contrast_list_4 <- list("Native  - Alien" =  Native  - Alien)

post_hoc_yawan_nw.biom_Alien <- contrast(emm.yawan_nw.biom_Alien, method = contrast_list_4)
post_hoc_yawan_nw.biom_Alien

plot(post_hoc_yawan_nw.biom_Alien) 


#Error bar plot with pairwise comparison
p_nw.biom_yawan_Alien <- ggplot(yawan_nw_biomass_Status) +
  aes(x = Status, y = prop_logit) + 
  geom_jitter( size=3, shape=20, col= "grey", width = 0.08) +
  stat_summary(fun.data = mean_ci, width=0.2, geom = "errorbar",linewidth = 1) +
  stat_summary(fun.y="mean", size=0.95) +
  labs(x="") + labs (y="") + ggtitle("1700m") + ylim(-4,4) +
  #geom_text(data = cldisplay_numba_w.biom_status, aes(y = 4, label = .group)) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1, vjust = 0, face = "bold")) +
  theme(axis.title.x =element_text(size=13, margin = margin(20,0), face="bold")) +
  theme(axis.title.y =element_text(size=13, margin = margin(0,8), face="bold")) ;p_nw.biom_yawan_Alien  




#combine all alien WP plants
prow <- cowplot::plot_grid(p_w.biom_numba_Alien,
                           p_w.biom_yawan_Alien,
                           ncol = 2, byrow = TRUE,labels = c('A', 'B'), align="hv") 


#combine all alien NWP plants
prow <- cowplot::plot_grid(p_nw.biom_numba_Alien,
                           p_nw.biom_yawan_Alien,
                           ncol = 2, byrow = TRUE,labels = c('A', 'B'), align="hv") 

#combine all plot
prow <- cowplot::plot_grid(p_w.biom_numba_Alien,
                           p_w.biom_yawan_Alien,
                           p_nw.biom_numba_Alien,
                           p_nw.biom_yawan_Alien,
                           ncol = 2, byrow = TRUE,labels = c('A', 'B','C','D'), align="hv") 


final_plot <- ggdraw(prow) +
  draw_label("Plant status", x = 0.5, y = 0, vjust = -1, angle = 0, size = 14) 

final_plot





