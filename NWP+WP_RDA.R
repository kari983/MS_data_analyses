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
library(ggpp)

#delete all the variables that are there in the environment
#rm(list=ls()) 

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
#Selecting top species for NUMBA
numba_top_WP_species1 <-  numba_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

numba_top_WP_species2 <- numba_top_WP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Gardens + Treatments, value.var = "Biomass")

numba_top_WP_species2[is.na(numba_top_WP_species2)] <- 0 #removing NAs
numba_top_WP_species2

numba_top_WP_spp  <- numba_top_WP_species2[,2:37]#data frame/matrix of response (Y) variables

numba_top_WP_env  <- numba_top_WP_species2[,1:2]

numba_top_WP_species2$mean <- rowSums(numba_top_WP_species2[,2:37])/ncol(numba_top_WP_species2[,2:37])
numba_top_WP_species2

#top 10 species
numba_top_WP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################
#Numba NWP data
numba_WP_species <-  numba_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

# Function to extract first 4 letters of genus and first 2 letters of species
extract_species_code <- function(Plant_sp) {
  # Split the scientific name into genus and species
  parts <- strsplit(Plant_sp, " ")[[1]]
  # Extract the first 4 letters of the genus
  genus <- substr(parts[1], 1, 1)
  # Extract the first 2 letters of the species
  species <- substr(parts[2], 1, 3)
  # Combine them
  paste0(genus, "",species)
}

# Apply the function to create the new column
numba_WP_species$species_code <- sapply(numba_WP_species$Plant_sp, extract_species_code)
numba_WP_species

numba_WP_species <- transform(numba_WP_species, species_code = toupper(species_code))
numba_WP_species

numba_WP_species2 <- numba_WP_species  %>% 
  reshape2::dcast(Gardens + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

numba_WP_species2[is.na(numba_WP_species2)] <- 0 #removing NAs
numba_WP_species2

numba_WP_env  <- numba_WP_species2[,1:2] #data frame/matrix of explanatory (X) variables

numba_WP_spp  <- numba_WP_species2[,-c(1:2)]#data frame/matrix of response (Y) variables


#STATISTICS
species.Hellinger1 <- disttransform(numba_WP_spp, method='log')  #log transformation on NWP biomass
#species.Hellinger1 <- decostand(numba_WP_spp, method='hellinger')  #log transformation on NWP biomass
Ordination.model1  <- rda(species.Hellinger1 ~ Treatments + Condition(Gardens), data= numba_WP_env, scaling=2)
#scaling="species" or scaling=1 means correlation between species variable. Scaling=2 is corr btw sites and species.

set.seed(10)

#Partitioning of variance
Ordination.model1
summary(Ordination.model1)
#Variation explained by each RDA axis

vif.cca(Ordination.model1)
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

#using adonis2 to find which terms are significant
adonis2(species.Hellinger1 ~ Treatments,data = numba_WP_env,method = "euclidean",permutations = 999,by = "onedf") 

#pairwise comparison
#library(pairwiseAdonis)
#set.seed(10)
pairwise.adonis(numba_WP_spp, numba_WP_env$Treatments)


#RDA PLOTTING for Numba Woody Biomass
#step 1
species.Hellinger1 <- disttransform(numba_WP_spp, method='log')   #disttransform log transformation
#species.Hellinger1 <- decostand(numba_NWP_spp, method='hellinger')  #log transformation on NWP biomass
Ordination.model1  <- rda(species.Hellinger1 ~ Treatments + Condition(Gardens), data= numba_WP_env, scaling=2)
#scaling="species" or scaling=1 shows similarities btw objects in response matrix. Scaling=2 shows effect of Treatments or explanatory variable


#spec.rda.AIC <- step(Ordination.model1, scope=formula(Ordination.model1, test = "perm"))
##choosing the best model based on AIC

summary(Ordination.model1)

plot1 <- ordiplot(Ordination.model1, choices=c(1,2))

#step 2
sites.long1 <- sites.long(plot1, env.data = numba_WP_env)
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
#species.long1_filter <- species.long1 %>%
  #filter(labels %in% c("STOR","MBIF","TPLE","MMAL",
                       #"FMOL","HNOV","FARF","PADU",
                       #"ECON","MALE")) %>% as.data.frame()

species.long1_filter <- species.long1 %>%
  filter(labels %in% c("STOR","MBIF","TPLE","MMAL","FMOL")) %>%
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
  labs(x="RDA1 WP [17.49%]") + labs (y="RDA2 WP [5.31%]") + ggtitle("700m") +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long1_axes, 
             aes(x=axis1, y=axis2, colour=Treatments), 
             size=5, show.legend = TRUE) + 
  geom_point(data=centroid1, size=3, shape=22, fill= c("red","darkgreen","black","blue"), color="black") +
  geom_segment(data=species.long1_filter, 
               aes(x=0, y=0, xend=axis1*1, yend=axis2*1), 
               colour="black", size=1, arrow=arrow(length = unit(3,"mm"))) +
  geom_text_repel(data=species.long1_filter, 
                  aes(x=axis1, y=axis2, label=labels), position = position_nudge_center(0.2, 0.1, 0, 0), 
                  colour="black", size=6.5) + 
  ggsci::scale_colour_npg() +
  #coord_fixed(ratio=1) + 
  geom_mark_ellipse(data=sites.long1, 
                    aes(x=axis1, y=axis2, 
                        color=Treatments, 
                        fill=after_scale(alpha(colour, 0.2))), # Use a transparent version of colour for fill
                    expand=0, show.legend=FALSE) + 
  theme_test() + 
  theme(legend.position = c(0.85, 0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.45,-1.1,1.8,1), 
                        y= c(1.2,-1.7,-0.3,-1.4), 
           label=c("C","I","W","WI"), size=7,
           color=c("red","darkgreen","black","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "darkgreen","black","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =22, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=17,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.4, face = "bold"),
        axis.title.y = element_text(hjust=0.6,face = "bold")) +
  coord_cartesian(xlim = c(-1.5, 2.1), ylim = c(-3, 2)); numba_WP_RDA_plot


#How to read RDA ordination plot
#Less then 90 degrees, they are positively correlated
#About 90 degrees, they are uncorrelated
#Greater then 90 degrees, they are negatively correlated


##############################################################################################################
# YAWAN WOODY BIOMASS
########################################################################################

yawan_biomass_data  <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/google drive_backup/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                                  sheet = "Yawan_biomass_2023")


##############################################################################################################
#Selecting top species for NUMBA
yawan_top_WP_species1 <-  yawan_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

yawan_top_WP_species2 <- yawan_top_WP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Gardens + Treatments, value.var = "Biomass")

yawan_top_WP_species2[is.na(yawan_top_WP_species2)] <- 0 #removing NAs
yawan_top_WP_species2

yawan_top_WP_spp  <- yawan_top_WP_species2[,2:37]#data frame/matrix of response (Y) variables

yawan_top_WP_env  <- yawan_top_WP_species2[,1:2]

yawan_top_WP_species2$mean <- rowSums(yawan_top_WP_species2[,2:37])/ncol(yawan_top_WP_species2[,2:37])
yawan_top_WP_species2

#top 10 species
yawan_top_WP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################

#Yawan woody biomass 
yawan_WP_species <-  yawan_biomass_data  %>%
  filter(Plants=="woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) %>% 
  arrange(desc(Biomass))

# Function to extract first 4 letters of genus and first 2 letters of species
extract_species_code <- function(Plant_sp) {
  # Split the scientific name into genus and species
  parts <- strsplit(Plant_sp, " ")[[1]]
  # Extract the first 4 letters of the genus
  genus <- substr(parts[1], 1, 1)
  # Extract the first 2 letters of the species
  species <- substr(parts[2], 1, 3)
  # Combine them
  paste0(genus, "",species)
}

# Apply the function to create the new column
yawan_WP_species$species_code <- sapply(yawan_WP_species$Plant_sp, extract_species_code)
yawan_WP_species

yawan_WP_species <- transform(yawan_WP_species, species_code = toupper(species_code))
yawan_WP_species

yawan_WP_species2 <- yawan_WP_species %>% 
  reshape2::dcast(Gardens + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

yawan_WP_species2[is.na(yawan_WP_species2)] <- 0 #removing NAs
yawan_WP_species2

yawan_WP_env  <- yawan_WP_species2[,1:2]

yawan_WP_spp  <- yawan_WP_species2[,-c(1:2)]

#STATISTICS
species.Hellinger2 <- disttransform(yawan_WP_spp, method = 'log')
#species.Hellinger2 <- decostand(yawan_NWP_spp, method = 'standardize')
Ordination.model2  <- rda(species.Hellinger2 ~ Treatments + Condition(Gardens), data= yawan_WP_env, scaling = 2)

set.seed(10)

#Partitioning of variance
Ordination.model2
summary(Ordination.model2)

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

#using adonis2 to find which terms are significant
#adonis2(species.Hellinger2 ~ Treatments,data = yawan_WP_env,method = "euclidean",permutations = 999,by = "onedf") 

#pairwise comparison
library(pairwiseAdonis)
set.seed(10)
pairwise.adonis(yawan_WP_spp, yawan_WP_env$Treatments)




#RDA PLOTTING for Yawan Woody Biomass
#step 1
species.Hellinger2 <- disttransform(yawan_WP_spp, method = 'log')
Ordination.model2  <- rda(species.Hellinger2 ~ Treatments + Condition(Gardens), data= yawan_WP_env, scaling = 2)

summary(Ordination.model2)

plot2 <- ordiplot(Ordination.model2, choices=c(1,2))

#step 2
sites.long2 <- sites.long(plot2, env.data = yawan_WP_env)
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

#species.long2_filter <- species.long2 %>%
  #filter(labels %in% c("Pipt.arg","Desm.seq","Homa.ner","Trem,ori",
                       #"Ficu.pun","Pipe.sub","Pipe.rec","Debr.lon",
                       #"Pile.mel","Saur.con")) %>% as.data.frame()

species.long2_filter <- species.long2 %>%
  filter(labels %in% c("PARG","DSEQ","HNER","TORI",
                       "FPUN")) %>% as.data.frame()


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
  xlab(axis.long2[1, "label"]) +
  ylab(axis.long2[2, "label"]) +  
  labs(x="RDA1 WP [37.28%]") + labs (y="RDA2 WP [5.43%]") + ggtitle("1700m") +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long2, 
             aes(x=axis1, y=axis2, colour=Treatments), 
             size=5, show.legend = TRUE) + 
  geom_point(data=centroid2, size=3, shape=22, fill= c("red", "black","darkorange","blue"), color="black") +
  geom_segment(data=species.long2_filter, 
               aes(x=0, y=0, xend=axis1, yend=axis2), 
               colour="black", size=1, arrow=arrow(length = unit(3,"mm"))) +
  geom_text_repel(data=species.long2_filter, 
                  aes(x=axis1*1, y=axis2*1, label=labels), position = position_nudge_center(0.1, 0.3, 0, 0), 
                  colour="black", size=6.5) + 
  ggsci::scale_colour_npg() +
  geom_mark_ellipse(data=sites.long2, 
                    aes(x=axis1, y=axis2, 
                        color=Treatments, 
                        fill=after_scale(alpha(colour, 0.2))), # Use a transparent version of colour for fill
                    expand=0, show.legend=FALSE) + 
  theme_test() +  
  theme(legend.position = c(0.15,0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.5,-0.5,0.5,1.0), 
                       y= c(0.65,-0.9,1.2,-1.2), 
           label=c("C","I","W","WI"), size=7,
           color=c("red", "black","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "black","darkorange","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =22, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=17,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.43, face = "bold"),
        axis.title.y = element_text(hjust=0.57,face = "bold")) +
  coord_cartesian(xlim = c(-1.35, 1.8), ylim = c(-3, 2.5)); yawan_WP_RDA_plot

############################################################################################################
############################################################################################################
#RDA CONTSRAINED ORDINATION FOR NON-WOODY BIOMASS

##############################################################################################################
# NUMBA NON-WOODY BIOMASS
########################################################################################
# load the data
numba_biomass_data <- read_excel("G:/My Drive/Garden Data_2023/For ANALYSIS/data/Numba_Biomass_2023.xlsx",
                                 sheet="Numba_biomass_2023")

##############################################################################################################
#Selecting top species for YAWAN
numba_top_NWP_species1 <-  numba_biomass_data  %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

numba_top_NWP_species2 <- numba_top_NWP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Gardens + Treatments, value.var = "Biomass")

numba_top_NWP_species2[is.na(numba_top_NWP_species2)] <- 0 #removing NAs
numba_top_NWP_species2

numba_top_NWP_spp  <- numba_top_NWP_species2[,2:37]#data frame/matrix of response (Y) variables

numba_top_NWP_env  <- numba_top_NWP_species2[,1:2]

numba_top_NWP_species2$mean <- rowSums(numba_top_NWP_species2[,2:37])/ncol(numba_top_NWP_species2[,2:37])
numba_top_NWP_species2

#top 10 species
numba_top_NWP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################
#Numba NWP data
numba_NWP_species <-  numba_biomass_data  %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

# Function to extract first 4 letters of genus and first 2 letters of species
extract_species_code <- function(Plant_sp) {
  # Split the scientific name into genus and species
  parts <- strsplit(Plant_sp, " ")[[1]]
  # Extract the first 4 letters of the genus
  genus <- substr(parts[1], 1, 1)
  # Extract the first 2 letters of the species
  species <- substr(parts[2], 1, 3)
  # Combine them
  paste0(genus, "",species)
}

# Apply the function to create the new column
numba_NWP_species$species_code <- sapply(numba_NWP_species$Plant_sp, extract_species_code)
numba_NWP_species

numba_NWP_species <- transform(numba_NWP_species, species_code = toupper(species_code))
numba_NWP_species

numba_NWP_species2 <- numba_NWP_species  %>% 
  reshape2::dcast(Gardens + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

numba_NWP_species2[is.na(numba_NWP_species2)] <- 0 #removing NAs
numba_NWP_species2

numba_NWP_env  <- numba_NWP_species2[,1:2] #data frame/matrix of explanatory (X) variables

numba_NWP_spp  <- numba_NWP_species2[,-c(1:2)]#data frame/matrix of response (Y) variables


#STATISTICS
species.Hellinger3 <- disttransform(numba_NWP_spp, method='log')  #log transformation on NWP biomass
Ordination.model3  <- rda(species.Hellinger3 ~ Treatments + Condition(Gardens), data= numba_NWP_env, scaling=2)
#scaling="species" or scaling=1 means correlation between species variable. Scaling=2 is corr btw sites and species.

set.seed(10)

#Partitioning of variance
Ordination.model3
summary(Ordination.model3)
#Variation explained by each RDA axis

vif.cca(Ordination.model3)
#no collinearity (no factors are inflating each other: they are all less than 20):rule of thumb


#What is the variance explained by the treatments?
RsquareAdj(Ordination.model3)$adj.r.squared #total variance explained by the RDA model

#Is the model significant?
anova.cca(Ordination.model3, permutations = 999) #permutations = 999

#Which axes are significant?
anova.cca(Ordination.model3, by = "axis",  permutations = 999) 

#Which terms are significant?
anova.cca(Ordination.model3, by = "terms", permutations= 999) 

#using adonis2 to find which terms are significant
#adonis2(species.Hellinger3 ~ Treatments,data = numba_NWP_env,method = "euclidean",permutations = 999,by = "onedf") 

#pairwise comparison
library(pairwiseAdonis)
set.seed(10)
pairwise.adonis(numba_NWP_spp, numba_NWP_env$Treatments)


#RDA PLOTTING for Numba Woody Biomass
#step 1
species.Hellinger3 <- disttransform(numba_NWP_spp, method='log')  #log transformation on NWP biomass
Ordination.model3  <- rda(species.Hellinger3 ~ Treatments + Condition(Gardens), data= numba_NWP_env, scaling=2)
#scaling="species" or scaling=1 means correlation between species variable. Scaling=2 is corr btw sites and species.

#spec.rda.AIC <- step(Ordination.model1, scope=formula(Ordination.model1, test = "perm"))
##choosing the best model based on AIC

summary(Ordination.model3)

plot3 <- ordiplot(Ordination.model3, choices=c(1,2))

#step 2
sites.long3 <- sites.long(plot3, env.data = numba_NWP_env)
head(sites.long3)

species.long3 <- species.long(plot3)
species.long3

axis.long3 <- axis.long(Ordination.model3, choices=c(1, 2))
axis.long3

#step 3
spec.envfit3 <- envfit(plot3, env=species.Hellinger3)
spec.data.envfit3 <- data.frame(r=spec.envfit3$vectors$r, p=spec.envfit3$vectors$pvals)
species.long3 <- species.long(plot3, spec.data=spec.data.envfit3)
species.long3

#species.long1 <- species.long1 %>%
#arrange(desc(r)) 

#species.long3_filter <- species.long3 %>%
  #filter(labels %in% c("Chro.odo","Pasp.con","Pasp.pan","Erig.sum",
                       #"Ager.con","Elep.mol","Musa.sp1","Sida.acu",
                       #"Arth.his","Stac.jam")) %>% as.data.frame()

species.long3_filter <- species.long3 %>%
filter(labels %in% c("CODO","PCON","PPAN","ESUM","ACON")) %>% as.data.frame()

#plotting
#Adding ordispider diagrams
centroid3 <- sites.long3 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long3_axes <- sites.long3[,1:4]


#plot with plant species
numba_NWP_RDA_plot <- ggplot(sites.long3_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long3[1, "label"]) +
  ylab(axis.long3[2, "label"]) +  
  labs(x="RDA1 NWP [49.08%]") + labs (y="RDA2 NWP [3.09%]") + ggtitle("700m") +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long3_axes, 
             aes(x=axis1, y=axis2, colour=Treatments), 
             size=5, show.legend = TRUE) + 
  geom_point(data=centroid3, size=3, shape=22, fill= c("red", "darkgreen","darkorange","blue"), color="black") +
  geom_segment(data=species.long3_filter, 
               aes(x=0, y=0, xend=axis1*1, yend=axis2*1), 
               colour="black", size=1, arrow=arrow(length = unit(3,"mm"))) +
  geom_text_repel(data=species.long3_filter, 
                  aes(x=axis1, y=axis2, label=labels), position = position_nudge_center(0.2, 0.1, 0, 0), 
                  colour="black", size=6.5) + 
  ggsci::scale_colour_npg() +
  #coord_fixed(ratio=1) + 
  geom_mark_ellipse(data=sites.long3, 
                    aes(x=axis1, y=axis2, 
                        color=Treatments, 
                        fill=after_scale(alpha(colour, 0.2))), # Use a transparent version of colour for fill
                    expand=0, show.legend=FALSE) + 
  theme_test() + 
  theme(legend.position = c(0.85, 0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.3,-1,0.65,0.4), 
           y= c(1.0,-1.0,0.55,-0.3), 
           label=c("C","I","W","WI"), size=7,
           color=c("red", "darkgreen","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "darkgreen","darkorange","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =22, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=17,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.55, face = "bold"),
        axis.title.y = element_text(hjust=0.5,face = "bold")) +
  coord_cartesian(xlim = c(-1.8, 1.55), ylim = c(-2, 2)); numba_NWP_RDA_plot


#How to read RDA ordination plot
#Less then 90 degrees, they are positively correlated
#About 90 degrees, they are uncorrelated
#Greater then 90 degrees, they are negatively correlated


##############################################################################################################
# YAWAN NON-WOODY BIOMASS
########################################################################################

yawan_biomass_data  <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/google drive_backup/For ANALYSIS/data/Yawan_Biomass_2023.xlsx",
                                  sheet = "Yawan_biomass_2023")


##############################################################################################################
#Selecting top species for NUMBA
yawan_top_NWP_species1 <-  yawan_biomass_data  %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

yawan_top_NWP_species2 <- yawan_top_NWP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Gardens + Treatments, value.var = "Biomass")

yawan_top_NWP_species2[is.na(yawan_top_NWP_species2)] <- 0 #removing NAs
yawan_top_NWP_species2

yawan_top_NWP_spp  <- yawan_top_NWP_species2[,2:37]#data frame/matrix of response (Y) variables

yawan_top_woody_env  <- yawan_top_NWP_species2[,1:2]

yawan_top_NWP_species2$mean <- rowSums(yawan_top_NWP_species2[,2:37])/ncol(yawan_top_NWP_species2[,2:37])
yawan_top_NWP_species2

#top 10 species
yawan_top_NWP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################

#Yawan woody biomass 
yawan_NWP_species <-  yawan_biomass_data  %>%
  filter(Plants=="non_woody") %>%
  group_by(Gardens, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg)) %>% 
  arrange(desc(Biomass))

#yawan_NWP_species <- transform(yawan_NWP_species, Plant_sp = as.character(Plant_sp))

# Function to extract first 4 letters of genus and first 2 letters of species
extract_species_code <- function(Plant_sp) {
  # Split the scientific name into genus and species
  parts <- strsplit(Plant_sp, " ")[[1]]
  # Extract the first 4 letters of the genus
  genus <- substr(parts[1], 1, 1)
  # Extract the first 2 letters of the species
  species <- substr(parts[2], 1, 3)
  # Combine them
  paste0(genus, "",species)
}

# Apply the function to create the new column
yawan_NWP_species$species_code <- sapply(yawan_NWP_species$Plant_sp, extract_species_code)
yawan_NWP_species

yawan_NWP_species <- transform(yawan_NWP_species, species_code = toupper(species_code))
yawan_NWP_species

yawan_NWP_species2 <- yawan_NWP_species %>% 
  reshape2::dcast(Gardens + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

yawan_NWP_species2[is.na(yawan_NWP_species2)] <- 0 #removing NAs
yawan_NWP_species2

yawan_NWP_env  <- yawan_NWP_species2[,1:2]

yawan_NWP_spp  <- yawan_NWP_species2[,-c(1:2)]


#STATISTICS
species.Hellinger4 <- disttransform(yawan_NWP_spp, method = 'log')
Ordination.model4  <- rda(species.Hellinger4 ~ Treatments + Condition(Gardens), data= yawan_NWP_env, scaling = 2)

set.seed(10)

#Partitioning of variance
Ordination.model4
summary(Ordination.model4)

vif.cca(Ordination.model4)
#no collinearity (no factors are inflating each other: they are all less than 20):rule of thumb


#What is the variance explained by the treatments?
RsquareAdj(Ordination.model4)$adj.r.squared
#The RDA’s adjusted R2 is 17.9%, and is significant (p = 0.001). 

#Is the model significant?
anova.cca(Ordination.model4, permutations = 999) #you can also use: permutations = 999

#Which axes are significant?
anova.cca(Ordination.model4, by = "axis",  permutations = 999) 

#Which terms are significant?
anova.cca(Ordination.model4, by = "terms", permutations= 999) 

#using adonis2 to find which terms are significant
#adonis2(species.Hellinger4 ~ Treatments,data = yawan_NWP_env,method = "euclidean",permutations = 999,by = "onedf") 

#pairwise comparison
library(pairwiseAdonis)
set.seed(10)
pairwise.adonis(yawan_NWP_spp, yawan_NWP_env$Treatments)



#RDA PLOTTING for Yawan Woody Biomass
#step 1
species.Hellinger4 <- disttransform(yawan_NWP_spp, method = 'log')
Ordination.model4  <- rda(species.Hellinger4 ~ Treatments + Condition(Gardens), data= yawan_NWP_env, scaling = 2)

summary(Ordination.model4)

plot4 <- ordiplot(Ordination.model4, choices=c(1,2))

#step 2
sites.long4 <- sites.long(plot4, env.data = yawan_NWP_env)
head(sites.long4)

species.long4 <- species.long(plot4)
species.long4

axis.long4 <- axis.long(Ordination.model4, choices=c(1, 2))
axis.long4

#step 3
spec.envfit4 <- envfit(plot4, env=species.Hellinger4)
spec.data.envfit4 <- data.frame(r=spec.envfit4$vectors$r, p=spec.envfit4$vectors$pvals)
species.long4 <- species.long(plot4, spec.data = spec.data.envfit4)
species.long4

#species.long2 <- species.long2 %>%
#arrange(desc(r)) 

#species.long4_filter <- species.long4 %>%
  #filter(labels %in% c("Seta.pal","Desm.int","Pasp.con","Micr.vim",
                       #"Bide.pil","Cicu.sp1","Phys.min","Aspl.sp1",
                       #"Pass.lin","Gunn.mac")) %>% as.data.frame()

species.long4_filter <- species.long4 %>%
  filter(labels %in% c("SPAL","DINT","PCON","MVIM","BPIL")) %>% as.data.frame()


#plotting
#Adding ordispider diagrams
centroid4 <- sites.long4 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long4_axes <- sites.long4[,1:4]


#Plots with top 10 species
yawan_NWP_RDA_plot <- ggplot(sites.long4_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long4[1, "label"]) +
  ylab(axis.long4[2, "label"]) +  
  labs(x="RDA1 NWP [53.85%]") + labs (y="RDA2 NWP [3.54%]") + ggtitle("1700m") +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long4, 
             aes(x=axis1, y=axis2, colour=Treatments), 
             size=5, show.legend = TRUE) + 
  geom_point(data=centroid4, size=3, shape=22, fill= c("red", "black","darkorange","blue"), color="black") +
  geom_segment(data=species.long4_filter, 
               aes(x=0, y=0, xend=axis1, yend=axis2), 
               colour="black", size=1, arrow=arrow(length = unit(3,"mm"))) +
  geom_text_repel(data=species.long4_filter, 
                  aes(x=axis1*1, y=axis2*1, label=labels), position = position_nudge_center(0.2, 0.3, 0, 0), 
                  colour="black", size=6.5) + 
  ggsci::scale_colour_npg() +
  geom_mark_ellipse(data=sites.long4, 
                    aes(x=axis1, y=axis2, 
                        color=Treatments, 
                        fill=after_scale(alpha(colour, 0.2))), # Use a transparent version of colour for fill
                    expand=0, show.legend=FALSE) + 
  theme_test() +  
  theme(legend.position = c(0.15,0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.5,-0.7,0.43,0.5), 
                       y= c(1.5,-1.2,-0.6,0.75), 
           label=c("C","I","W","WI"), size=7,
           color=c("red", "black","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "black","darkorange","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =22, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=17,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.8, face = "bold"),
        axis.title.y = element_text(hjust=0.5,face = "bold")) +
  coord_cartesian(xlim = c(-2.5, 0.8), ylim = c(-3, 3)); yawan_NWP_RDA_plot


#####---------------------------------------------------------
#combine ordination plots
#####---------------------------------------------------------
#combine ordination plots

library(grid)
library(cowplot)


RDA.combine.spp.plot <-  cowplot::plot_grid(numba_NWP_RDA_plot,  
                                            numba_WP_RDA_plot,
                                            yawan_NWP_RDA_plot,
                                            yawan_WP_RDA_plot, 
                                            ncol = 2, byrow = TRUE,labels = c('A', 'B', 'C', 'D'),
                                            label_size = 22,align="hv"); RDA.combine.spp.plot

#Saving ordination plot as jpg format
#ggsave("RDA.combine.spp.plot.jpg", width = 40, height = 37, units = "cm")

#Saving ordination plot in tiff format (dpi = 600)
ggsave("RDA.combine.spp.plot.tiff", width = 40, height = 37, units = "cm", dpi = 600)



