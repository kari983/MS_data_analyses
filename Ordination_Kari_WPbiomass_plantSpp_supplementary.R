#Load packages
library(ggplot2)
library(ggpubr)
library(CCA)
library(readxl)
library(vegan)
library(BiodiversityR)
library(ggplot2)
library(ggforce)
library(reshape2)
library(dplyr)
library(ggrepel)

#delete all the variables that are there in the environment
rm(list=ls()) 

############################################################################################################
############################################################################################################
#RDA CONTSRAINED ORDINATION FOR WOODY BIOMASS

##############################################################################################################
# NUMBA WOODY BIOMASS
########################################################################################
# load the data
numba_biomass_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                                 sheet="Numba_biomass_2023")

##############################################################################################################
#Selecting top species for YAWAN
numba_top_woody_species1 <-  numba_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

numba_top_woody_species2 <- numba_top_woody_species1  %>% 
  reshape2::dcast(Plant_sp ~ Gardens + Treatments, value.var = "Biomass")

numba_top_woody_species2[is.na(numba_top_woody_species2)] <- 0 #removing NAs
numba_top_woody_species2

numba_top_woody_spp  <- numba_top_woody_species2[,2:37]#data frame/matrix of response (Y) variables

numba_top_woody_env  <- numba_top_woody_species2[,1:2]

numba_top_woody_species2$mean <- rowSums(numba_top_woody_species2[,2:37])/ncol(numba_top_woody_species2[,2:37])
numba_top_woody_species2

#top 10 species
numba_top_woody_species2[,-c(2:37)] %>%
  slice_max(mean, n=10)

##############################################################################################################
#Numba Woody data
numba_woody_species <-  numba_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

#numba_woody_species2 <- numba_woody_species  %>%  
  #slice_max(Biomass, n = 5) #selecting 5 plants with highest biomass in each treatment

#numba_woody_species2 <- numba_woody_species %>% 
 #filter(Plant_sp %in% c("Piper aduncum","Solanum torvum","Macaranga bifoveata","Falcataria moluccana",'Trichospermum pleiostigma')) 

numba_woody_species3 <- numba_woody_species  %>% 
  reshape2::dcast(Gardens + Treatments ~ Plant_sp, value.var = "Biomass")

numba_woody_species3[is.na(numba_woody_species3)] <- 0 #removing NAs
numba_woody_species3

numba_woody_spp  <- numba_woody_species3[,3:117]#data frame/matrix of response (Y) variables
#numba_woody_spp  <- numba_woody_species3[,3:7]
#numba_woody_spp  <- numba_woody_species3[,3:5]

numba_woody_env  <- numba_woody_species3[,1:2] #data frame/matrix of explanatory (X) variables

#library("writexl")
#df1 <- as.data.frame(numba_woody_env)
#write_xlsx(df1,"C:/Users/Kari Iamba/Downloads//numba_woody_env.xlsx")


#STATISTICS
species.Hellinger1 <- disttransform(numba_woody_spp, method='log')  #log transformation on woody biomass
#species.Hellinger1 <- decostand(numba_woody_spp, method='standardize')  
#species.Hellinger1 <- decostand(numba_woody_spp, method='hellinger') #hellinger transformation on woody biomass
#data. Turns absolute biomass into relative biomass.
Ordination.model1  <- rda(species.Hellinger1 ~ Treatments + Condition(Gardens), data= numba_woody_env, scaling=2)

#Ordination.model1  <- dbrda(species.Hellinger1 ~ Treatments + Condition(Gardens), data= numba_woody_env, scaling=2)

#scaling="species" or scaling=1 means correlation between species variable. Scaling=2 is corr btw sites and species.

set.seed(10)

#Partitioning of variance
Ordination.model1
summary(Ordination.model1)
#RDA1 explains 24.3% of the variation, RDA2 explains 6.6 %. Total explained by these 2-axis is 30.84%
#Treatments explained 21.33% of the variation in woody plant biomass but 43.63% of the variation 
#is not explained by the treatments (explanatory variables) :[look at Partitioning of variance]

vif.cca(Ordination.model1 )
#no collinearity (no factors are inflating each other: they are all less than 20):rule of thumb


#What is the variance explained by the treatments?
RsquareAdj(Ordination.model1)$adj.r.squared #total variance explained by the RDA model
#The RDA’s adjusted R2 is 21.8%, and is significant (p = 0.001). 

#Is the model significant?
anova.cca(Ordination.model1, permutations = 999, adjust="tukey") #can also use: permutations = 999

#Which axes are significant?
anova.cca(Ordination.model1, by = "axis", permutations = 999, adjust="tukey") 

#Which terms are significant?
anova.cca(Ordination.model1, by = "terms", permutations= 999, adjust="tukey") 

#sequential test for contrasts
anova.cca(Ordination.model1, by = "onedf", permutations = 999, adjust="tukey")
#'arg' should be one of “terms”, “margin”, “axis”, “onedf”

#using adonis2 to find which terms are significant
#adonis2(species.Hellinger1 ~ Treatments,data = numba_woody_env,method = "euclidean",permutations = 999,by = "onedf") 

#pairwise comparison
#library(pairwiseAdonis)
#set.seed(10)
#pairwise.adonis(species.Hellinger1, numba_woody_env$Treatments)



#RDA PLOTTING for Numba Woody Biomass
#step 1
# script generated by the BiodiversityR GUI from the constrained ordination menu
species.Hellinger1 <- disttransform(numba_woody_spp, method='log')   #disttransform log transformation
#species.Hellinger1 <- decostand(numba_woody_spp, method='standardize')
Ordination.model1  <- rda(species.Hellinger1 ~ Treatments + Condition(Gardens), data= numba_woody_env, scaling=2)
#scaling="species" or scaling=1 shows similarities btw objects in response matrix. Scaling=2 shows effect of Treatments or explanatory variable


#spec.rda.AIC <- step(Ordination.model1, scope=formula(Ordination.model1, test = "perm"))
##choosing the best model based on AIC

summary(Ordination.model1)

plot1 <- ordiplot(Ordination.model1, choices=c(1,2))

#step 2
sites.long1 <- sites.long(plot1, env.data = numba_woody_env)
head(sites.long1)

species.long1 <- species.long(plot1)
species.long1

axis.long1 <- axis.long(Ordination.model1, choices=c(1, 2))
axis.long1

#step 3
spec.envfit1 <- envfit(plot1, env=species.Hellinger1)
spec.data.envfit1 <- data.frame(r=spec.envfit1$vectors$r, p=spec.envfit1$vectors$pvals)
species.long1 <- species.long(plot1, spec.data=spec.data.envfit1)
species.long1

#species.long1 <- species.long1 %>%
           #arrange(desc(r)) 

species.long1_filter <- species.long1 %>%
  filter(labels %in% c("Solanum.torvum","Macaranga.bifoveata","Trichospermum.pleiostigma","Melastoma.malabathricum",
                       "Falcataria.moluccana","Homalanthus.novoguineensis","Ficus.arfakensis","Piper.aduncum",
                       "Embellia.continoides","Macaranga.aleuritoides")) %>%
  as.data.frame()

#plotting
#Adding ordispider diagrams
centroid1 <- sites.long1 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long1_axes <- sites.long1[,1:4]


#plot with plant species
numba_WP_RDA_plot <- ggplot(sites.long1_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long1[1, "label"]) +
  ylab(axis.long1[2, "label"]) +  
  labs(x="RDA1 [17.47%]") + labs (y="RDA2 [5.29%]") + ggtitle("") +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long1_axes, 
             aes(x=axis1, y=axis2, colour=Treatments), 
             size=5, show.legend = TRUE) + 
  geom_segment(data=species.long1_filter, 
               aes(x=0, y=0, xend=axis1*1, yend=axis2*1), 
               colour="black", size=1, arrow=arrow(length = unit(3,"mm"))) +
  geom_text_repel(data=species.long1_filter, 
                  aes(x=axis1*1.1, y=axis2*1.1, label=labels), max.overlaps = Inf,
                  colour="black", size=3.5) + 
  ggsci::scale_colour_npg() +
  #coord_fixed(ratio=1) + 
  geom_mark_ellipse(data=sites.long1, 
                    aes(x=axis1, y=axis2, 
                        color=Treatments, 
                        fill=after_scale(alpha(colour, 0.2))), # Use a transparent version of colour for fill
                    expand=0, show.legend=FALSE) + 
  theme_test() + theme(plot.title=element_text(hjust=0.5)) + 
  xlim(-2.2,2.2) + ylim(-2.5,1.5) +
  theme(legend.position = c(0.1, 0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.4,-1.0,1.8,0.9), 
                       y= c(0.8,-1.4,-0.5,-1), 
                        label=c("C","I","W","WI"), size=7,
                        color=c("red", "darkgreen","grey","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "darkgreen","grey","blue")); numba_WP_RDA_plot 


#How to read RDA ordination plot
#Less then 90 degrees, they are positively correlated
#About 90 degrees, they are uncorrelated
#Greater then 90 degrees, they are negatively correlated


##############################################################################################################
# YAWAN WOODY BIOMASS
########################################################################################
#yawan_biomass_data  <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
# sheet = "Yawan_biomass_2023")

yawan_biomass_data  <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/google drive_backup/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                                  sheet = "Yawan_biomass_2023")


##############################################################################################################
#Selecting top species for NUMBA
yawan_top_woody_species1 <-  yawan_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

yawan_top_woody_species2 <- yawan_top_woody_species1  %>% 
  reshape2::dcast(Plant_sp ~ Gardens + Treatments, value.var = "Biomass")

yawan_top_woody_species2[is.na(yawan_top_woody_species2)] <- 0 #removing NAs
yawan_top_woody_species2

yawan_top_woody_spp  <- yawan_top_woody_species2[,2:37]#data frame/matrix of response (Y) variables

yawan_top_woody_env  <- yawan_top_woody_species2[,1:2]

yawan_top_woody_species2$mean <- rowSums(yawan_top_woody_species2[,2:37])/ncol(yawan_top_woody_species2[,2:37])
yawan_top_woody_species2

#top 10 species
yawan_top_woody_species2[,-c(2:37)] %>%
  slice_max(mean, n=10)

##############################################################################################################

#Yawan woody biomass 
yawan_Woody_species <-  yawan_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) %>% 
  arrange(desc(Biomass))


#yawan_Woody_species2 <- yawan_Woody_species %>%  
  #slice_max(Biomass, n = 5) #selecting 5 plants with highest biomass in each treatment

#yawan_Woody_species2 <- yawan_Woody_species %>% 
#filter(Plant_sp %in% c("Desmodium sequax","Pipturus argenteus","Pilea melastomoides",
                       #"Ficus pungens","Homalanthus nervosus")) 


yawan_woody_species3 <- yawan_Woody_species %>% 
  reshape2::dcast(Gardens + Treatments ~ Plant_sp, value.var = "Biomass")


yawan_woody_species3[is.na(yawan_woody_species3)] <- 0 #removing NAs
yawan_woody_species3

#yawan_woody_spp  <- yawan_woody_species3[,3:7]
yawan_woody_spp  <- yawan_woody_species3[,3:105]


yawan_woody_env  <- yawan_woody_species3[,1:2]


#STATISTICS
species.Hellinger2 <- decostand(yawan_woody_spp, method='standardize')
#species.Hellinger2 <- disttransform(yawan_woody_spp, method = 'log')
Ordination.model2  <- rda(species.Hellinger2 ~ Treatments + Condition(Gardens), data= yawan_woody_env, scaling = 2, 
                          correlation=TRUE)

set.seed(10)

#Partitioning of variance
Ordination.model2
summary(Ordination.model2)
#RDA1 explains 17.3% of the variation, RDA2 explains 9.4 %. Total explained by these 2-axis is 26.7 %
#Treatments explained 19.6% of the variation in woody plant biomass but 80.4% of the variation 
#is not explained by the treatments (explanatory variables) :[look at Partitioning of variance]

vif.cca(Ordination.model2)
#no collinearity (no factors are inflating each other: they are all less than 20):rule of thumb


#What is the variance explained by the treatments?
RsquareAdj(Ordination.model2)$adj.r.squared
#The RDA’s adjusted R2 is 17.9%, and is significant (p = 0.001). 

#Is the model significant?
anova.cca(Ordination.model2, permutations = 999) #you can also use: permutations = 999

#Which axes are significant?
anova.cca(Ordination.model2, by = "axis",  permutations = 999) 

#Which terms are significant?
anova.cca(Ordination.model2, by = "terms", permutations= 999) 

#Which terms (treatments) are significant?
anova.cca(Ordination.model2, by = "onedf", permutations= 999, adjust="tukey")
#'arg' should be one of “terms”, “margin”, “axis”, “onedf”

#using adonis2 to find which terms are significant
#adonis2(species.Hellinger2 ~ Treatments,data = yawan_woody_env,method = "euclidean",permutations = 999,by = "onedf") 

#pairwise comparison
#library(pairwiseAdonis)
#set.seed(10)
#species.log_transform <- disttransform(yawan_woody_spp, method = 'log')
#pairwise.adonis(species.log_transform, yawan_woody_env$Treatments)


#RDA PLOTTING for Yawan Woody Biomass
#step 1
# script generated by the BiodiversityR GUI from the constrained ordination menu
species.Hellinger2 <- disttransform(yawan_woody_spp, method='log')
#species.Hellinger2 <- decostand(yawan_woody_spp, method = 'standardize')
Ordination.model2  <- rda(species.Hellinger2 ~ Treatments + Condition(Gardens), data= yawan_woody_env, scaling = 2)

summary(Ordination.model2)

plot2 <- ordiplot(Ordination.model2, choices=c(1,2))

#step 2
sites.long2 <- sites.long(plot2, env.data = yawan_woody_env)
head(sites.long2)

species.long2 <- species.long(plot2)
species.long2

axis.long2 <- axis.long(Ordination.model2, choices=c(1, 2))
axis.long2

#step 3
spec.envfit2 <- envfit(plot2, env=species.Hellinger2)
spec.data.envfit2 <- data.frame(r=spec.envfit2$vectors$r, p=spec.envfit2$vectors$pvals)
species.long2 <- species.long(plot2, spec.data = spec.data.envfit2)
species.long2

#species.long2 <- species.long2 %>%
  #arrange(desc(r)) 

species.long2_filter <- species.long2 %>%
  filter(labels %in% c("Pipturus.argenteus","Desmodium.sequax","Homalanthus.nervosus","Trema.orientale",
                       "Ficus.pungens","Piper.subulatum","Piper.recessum","Debregeasia.longifolia",
                       "Pilea.melastomoides","Saurauia.conferta")) %>%
  as.data.frame()


#plotting
#Adding ordispider diagrams
centroid2 <- sites.long2 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long2_axes <- sites.long2[,1:4]


#Plots with top 10 species
yawan_WP_RDA_plot <- ggplot(sites.long2_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long1[1, "label"]) +
  ylab(axis.long1[2, "label"]) +  
  labs(x="RDA1 [37.28%]") + labs (y="RDA2 [5.43%]") + ggtitle("") +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long2, 
             aes(x=axis1, y=axis2, colour=Treatments), 
             size=5, show.legend = TRUE) + 
  geom_segment(data=species.long2_filter, 
               aes(x=0, y=0, xend=axis1*1.1, yend=axis2*1.1), 
               colour="black", size=1, arrow=arrow(length = unit(3,"mm"))) +
  geom_text_repel(data=species.long2_filter, 
                  aes(x=axis1*1.05, y=axis2*1.05, label=labels), max.overlaps = Inf,
                  colour="black", size=3.5) + 
  ggsci::scale_colour_npg() +
  geom_mark_ellipse(data=sites.long2, 
                    aes(x=axis1, y=axis2, 
                        color=Treatments, 
                        fill=after_scale(alpha(colour, 0.2))), # Use a transparent version of colour for fill
                    expand=0, show.legend=FALSE) + 
  xlim(-1.4,1.8) + ylim(-2.5,2) +  
  theme_test() +  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position = c(0.18,0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-1.32,-0.45,0.6,0.9), 
                       y= c(-0.2,-0.8,1.2,-1), 
                       label=c("C","I","W","WI"), size=7,
                       color=c("red", "grey","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "grey","darkorange","blue")); yawan_WP_RDA_plot 


#Saving ordination plot
#ggsave("yawan_WP_RDA_plot.jpg", width = 23, height = 20, units = "cm")


#####---------------------------------------------------------
#combine ordination plots
#####---------------------------------------------------------
#combine ordination plots

library(grid)
library(cowplot)

cowplot::plot_grid(numba_WP_RDA_plot,
                   yawan_WP_RDA_plot, 
                   ncol = 2, byrow = TRUE,labels = c('A', 'B'), align="hv") 

#combine plots 
prow <- plot_grid(
  numba_WP_RDA_plot,
  yawan_WP_RDA_plot,
  align = 'vh',
  labels = c("A", "B"),
  hjust = -2, 
  nrow = 1) 

legend <- get_legend(numba_WP_RDA_plot + 
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom")) 


RDA.spp.plot <- plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1), vjust = 1); RDA.spp.plot


#Saving ordination plot
ggsave("RDA.spp.plot.jpg", width = 37, height = 22, units = "cm")






