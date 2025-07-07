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

#Load combined site data
combine_elev.biomass_data <- read_excel("C:/Users/Kari Iamba/Desktop/Garden Final Data_2023/Manuscript/Finalized version/Final_MS_R_codes/datasets/Combine_Sites_Biomass_2023.xlsx",
                                        sheet="combine.site.biomass")

############################################################################################################
############################################################################################################
#RDA CONTSRAINED ORDINATION FOR WOODY BIOMASS

##############################################################################################################
# WOODY BIOMASS AT 700M
########################################################################################
#Selecting top WP species at 700m
Elev.700m_top_WP_species1 <-  combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

Elev.700m_top_WP_species2 <- Elev.700m_top_WP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Blocks + Treatments, value.var = "Biomass")

Elev.700m_top_WP_species2[is.na(Elev.700m_top_WP_species2)] <- 0 #removing NAs
Elev.700m_top_WP_species2

Elev.700m_top_WP_spp  <- Elev.700m_top_WP_species2[,2:37]#data frame/matrix of response (Y) variables

Elev.700m_top_WP_env  <- Elev.700m_top_WP_species2[,1:2]

Elev.700m_top_WP_species2$mean <- rowSums(Elev.700m_top_WP_species2[,2:37])/ncol(Elev.700m_top_WP_species2[,2:37])
Elev.700m_top_WP_species2

#top 5 species
Elev.700m_top_WP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################
#WP species biomass at 700m
Elev.700m_WP_species <-  combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
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
Elev.700m_WP_species$species_code <- sapply(Elev.700m_WP_species$Plant_sp, extract_species_code)
Elev.700m_WP_species

Elev.700m_WP_species <- transform(Elev.700m_WP_species, species_code = toupper(species_code))
Elev.700m_WP_species

Elev.700m_WP_species2 <- Elev.700m_WP_species  %>% 
  reshape2::dcast(Blocks + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

Elev.700m_WP_species2[is.na(Elev.700m_WP_species2)] <- 0 #removing NAs
Elev.700m_WP_species2

Elev.700m_WP_env  <- Elev.700m_WP_species2[,1:2] #data frame/matrix of explanatory (X) variables

Elev.700m_WP_spp  <- Elev.700m_WP_species2[,-c(1:2)]#data frame/matrix of response (Y) variables


#STATISTICS
species.matrix1 <- disttransform(Elev.700m_WP_spp, method='log')  #log transformation on NWP biomass
Ordination.model1  <- rda(species.matrix1 ~ Treatments + Condition(Blocks), data= Elev.700m_WP_env, scaling=2)
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


#pairwise comparison (multiconstrained function in BiodiversityR package)
set.seed(10)
multiconstrained(method = "rda", 
                 formula = species.matrix1 ~ Treatments + Condition(Blocks), 
                 data = Elev.700m_WP_env,  scaling=2,
                 distance = "bray")

#RDA PLOTTING for Woody Biomass at 700m
#step 1
species.matrix1 <- disttransform(Elev.700m_WP_spp, method='log')  #log transformation on NWP biomas
Ordination.model1  <- rda(species.matrix1 ~ Treatments + Condition(Blocks), data= Elev.700m_WP_env, scaling=2)
#scaling="species" or scaling=1 shows similarities btw objects in response matrix. Scaling=2 shows effect of Treatments or explanatory variable


#spec.rda.AIC <- step(Ordination.model1, scope=formula(Ordination.model1, test = "perm"))
##choosing the best model based on AIC

summary(Ordination.model1)

plot1 <- ordiplot(Ordination.model1, choices=c(1,2))

#step 2
sites.long1 <- sites.long(plot1, env.data = Elev.700m_WP_env)
head(sites.long1)

species.long1 <- species.long(plot1)
species.long1

axis.long1 <- axis.long(Ordination.model1, choices=c(1, 2))
axis.long1

#step 3
spec.envfit1 <- envfit(plot1, env=species.matrix1)
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
#Adding centroids
centroid1 <- sites.long1 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long1_axes <- sites.long1[,1:4]


#plot with plant species
Elev700m_WP_RDA_plot <- ggplot(sites.long1_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
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
  theme_classic() +
  theme(legend.position = c(0.85, 0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.45,-1.1,1.8,1), 
                        y= c(1.2,-1.7,-0.3,-1.4), 
           label=c("C","I","W","WI"), size=7,
           color=c("red","darkgreen","black","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "darkgreen","black","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =27, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=20,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.4, face = "bold"),
        axis.title.y = element_text(hjust=0.6,face = "bold")) +
  coord_cartesian(xlim = c(-1.5, 2.1), ylim = c(-3, 2)); Elev700m_WP_RDA_plot


#How to read RDA ordination plot
#Less then 90 degrees, they are positively correlated
#About 90 degrees, they are uncorrelated
#Greater then 90 degrees, they are negatively correlated


##############################################################################################################
# WOODY BIOMASS AT 1700m
########################################################################################

#Selecting top woody species for 1700m
Elev.1700m_top_WP_species1 <-  combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

Elev.1700m_top_WP_species2 <- Elev.1700m_top_WP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Blocks + Treatments, value.var = "Biomass")

Elev.1700m_top_WP_species2[is.na(Elev.1700m_top_WP_species2)] <- 0 #removing NAs
Elev.1700m_top_WP_species2

Elev.1700m_top_WP_spp  <- Elev.1700m_top_WP_species2[,2:37]#data frame/matrix of response (Y) variables

Elev.1700m_top_WP_env  <- Elev.1700m_top_WP_species2[,1:2]

Elev.1700m_top_WP_species2$mean <- rowSums(Elev.1700m_top_WP_species2[,2:37])/ncol(Elev.1700m_top_WP_species2[,2:37])
Elev.1700m_top_WP_species2

#top 5 species
Elev.1700m_top_WP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################

#Woody species biomass at 1700m
Elev.1700m_WP_species <-  combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Plants %in% "woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
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
Elev.1700m_WP_species$species_code <- sapply(Elev.1700m_WP_species$Plant_sp, extract_species_code)
Elev.1700m_WP_species

Elev.1700m_WP_species <- transform(Elev.1700m_WP_species, species_code = toupper(species_code))
Elev.1700m_WP_species

Elev.1700m_WP_species2 <- Elev.1700m_WP_species %>% 
  reshape2::dcast(Blocks + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

Elev.1700m_WP_species2[is.na(Elev.1700m_WP_species2)] <- 0 #removing NAs
Elev.1700m_WP_species2

Elev.1700m_WP_env  <- Elev.1700m_WP_species2[,1:2]

Elev.1700m_WP_spp  <- Elev.1700m_WP_species2[,-c(1:2)]

#STATISTICS
species.matrix2 <- disttransform(Elev.1700m_WP_spp, method = 'log')
Ordination.model2  <- rda(species.matrix2 ~ Treatments + Condition(Blocks), data= Elev.1700m_WP_env, scaling = 2)

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

#pairwise comparison (multiconstrained function in BiodiversityR package)
set.seed(10)
multiconstrained(method = "rda", 
                 formula = species.matrix2 ~ Treatments  + Condition(Blocks), 
                 data = Elev.1700m_WP_env, scaling=2,
                 distance = "bray", 
                 add = TRUE)

#RDA PLOTTING for Woody species biomass at 1700m
#step 1
species.matrix2 <- disttransform(Elev.1700m_WP_spp, method = 'log')
Ordination.model2  <- rda(species.matrix2 ~ Treatments + Condition(Blocks), data= Elev.1700m_WP_env, scaling = 2)

summary(Ordination.model2)

plot2 <- ordiplot(Ordination.model2, choices=c(1,2))

#step 2
sites.long2 <- sites.long(plot2, env.data = Elev.1700m_WP_env)
head(sites.long2)

species.long2 <- species.long(plot2)
species.long2

axis.long2 <- axis.long(Ordination.model2, choices=c(1, 2))
axis.long2

#step 3
spec.envfit2 <- envfit(plot2, env=species.matrix2)
spec.data.envfit2 <- data.frame(r=spec.envfit2$vectors$r, p=spec.envfit2$vectors$pvals)
species.long2 <- species.long(plot2, spec.data = spec.data.envfit2)
species.long2

#species.long2 <- species.long2 %>%
#arrange(desc(r)) 

species.long2_filter <- species.long2 %>%
  filter(labels %in% c("PARG","DSEQ","HNER","TORI",
                       "FPUN")) %>% as.data.frame()


#plotting
#Adding centroids
centroid2 <- sites.long2 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long2_axes <- sites.long2[,1:4]


#Plots with top 10 species
Elev1700m_WP_RDA_plot <- ggplot(sites.long2_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
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
  theme_classic() + 
  theme(legend.position = c(0.15,0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.5,-0.5,0.5,1.0), 
                       y= c(0.65,-0.9,1.2,-1.2), 
           label=c("C","I","W","WI"), size=7,
           color=c("red", "black","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "black","darkorange","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =27, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=20,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.43, face = "bold"),
        axis.title.y = element_text(hjust=0.57,face = "bold")) +
  coord_cartesian(xlim = c(-1.35, 1.8), ylim = c(-3, 2.5)); Elev1700m_WP_RDA_plot

############################################################################################################
############################################################################################################
#RDA CONTSRAINED ORDINATION FOR NON-WOODY BIOMASS

##############################################################################################################
# NON-WOODY BIOMASS AT 700M
########################################################################################
#Selecting top species for 700m
Elev.700m_top_NWP_species1 <-  combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

Elev.700m_top_NWP_species2 <- Elev.700m_top_NWP_species1  %>% 
  reshape2::dcast(Plant_sp ~ Blocks + Treatments, value.var = "Biomass")

Elev.700m_top_NWP_species2[is.na(Elev.700m_top_NWP_species2)] <- 0 #removing NAs
Elev.700m_top_NWP_species2

Elev.700m_top_NWP_spp  <- Elev.700m_top_NWP_species2[,2:37]#data frame/matrix of response (Y) variables

Elev.700m_top_NWP_env  <- Elev.700m_top_NWP_species2[,1:2]

Elev.700m_top_NWP_species2$mean <- rowSums(Elev.700m_top_NWP_species2[,2:37])/ncol(Elev.700m_top_NWP_species2[,2:37])
Elev.700m_top_NWP_species2

#top 5 species
Elev.700m_top_NWP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################
#NWP species biomass at 700m
Elev.700m_NWP_species <- combine_elev.biomass_data %>%
  filter(Elev %in% "700m",Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
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
Elev.700m_NWP_species$species_code <- sapply(Elev.700m_NWP_species$Plant_sp, extract_species_code)
Elev.700m_NWP_species

Elev.700m_NWP_species <- transform(Elev.700m_NWP_species, species_code = toupper(species_code))
Elev.700m_NWP_species

Elev.700m_NWP_species2 <- Elev.700m_NWP_species %>% 
  reshape2::dcast(Blocks + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

Elev.700m_NWP_species2[is.na(Elev.700m_NWP_species2)] <- 0 #removing NAs
Elev.700m_NWP_species2

Elev.700m_NWP_env  <- Elev.700m_NWP_species2[,1:2] #data frame/matrix of explanatory (X) variables

Elev.700m_NWP_spp  <- Elev.700m_NWP_species2[,-c(1:2)]#data frame/matrix of response (Y) variables


#STATISTICS
species.matrix3 <- disttransform(Elev.700m_NWP_spp, method='log')  
Ordination.model3  <- rda(species.matrix3 ~ Treatments + Condition(Blocks), data= Elev.700m_NWP_env, scaling=2)

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

#pairwise comparison (multiconstrained function in BiodiversityR package)
set.seed(10)
multiconstrained(method = "rda", 
                 formula = Elev.700m_NWP_spp ~ Treatments + Condition(Blocks), 
                 data = Elev.700m_NWP_env,  
                 distance = "bray", scaling=2,
                 add = TRUE)

#RDA PLOTTING for Numba Woody Biomass
#step 1
species.matrix3 <- disttransform(Elev.700m_NWP_spp, method='log')  
Ordination.model3  <- rda(species.matrix3 ~ Treatments + Condition(Blocks), data= Elev.700m_NWP_env, scaling=2)

#spec.rda.AIC <- step(Ordination.model1, scope=formula(Ordination.model1, test = "perm"))
##choosing the best model based on AIC

summary(Ordination.model3)

plot3 <- ordiplot(Ordination.model3, choices=c(1,2))

#step 2
sites.long3 <- sites.long(plot3, env.data = Elev.700m_NWP_env)
head(sites.long3)

species.long3 <- species.long(plot3)
species.long3

axis.long3 <- axis.long(Ordination.model3, choices=c(1, 2))
axis.long3

#step 3
spec.envfit3 <- envfit(plot3, env=species.matrix3)
spec.data.envfit3 <- data.frame(r=spec.envfit3$vectors$r, p=spec.envfit3$vectors$pvals)
species.long3 <- species.long(plot3, spec.data=spec.data.envfit3)
species.long3

#species.long1 <- species.long1 %>%
#arrange(desc(r)) 

species.long3_filter <- species.long3 %>%
filter(labels %in% c("CODO","PCON","PPAN","ESUM","ACON")) %>% as.data.frame()

#plotting
#Adding centroids
centroid3 <- sites.long3 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long3_axes <- sites.long3[,1:4]


#plot with plant species
Elev700m_NWP_RDA_plot <- ggplot(sites.long3_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
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
  theme_classic() + 
  theme(legend.position = c(0.85, 0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.3,-1,0.65,0.4), 
           y= c(1.0,-1.0,0.55,-0.3), 
           label=c("C","I","W","WI"), size=7,
           color=c("red", "darkgreen","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "darkgreen","darkorange","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =27, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=20,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.55, face = "bold"),
        axis.title.y = element_text(hjust=0.5,face = "bold")) +
  coord_cartesian(xlim = c(-1.8, 1.55), ylim = c(-2, 2)); Elev700m_NWP_RDA_plot 


##############################################################################################################
# NON-WOODY BIOMASS AT 1700M
########################################################################################

#Selecting top NWP species for 1700m
Elev.1700m_top_NWP_species1 <-  combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
  summarise(Biomass = sum(Biomass_kg))  %>%
  arrange(desc(Biomass))

Elev.1700m_top_NWP_species2 <- Elev.1700m_top_NWP_species1 %>% 
  reshape2::dcast(Plant_sp ~ Blocks + Treatments, value.var = "Biomass")

Elev.1700m_top_NWP_species2[is.na(Elev.1700m_top_NWP_species2)] <- 0 #removing NAs
Elev.1700m_top_NWP_species2

Elev.1700m_top_NWP_spp  <- Elev.1700m_top_NWP_species2[,2:37]#data frame/matrix of response (Y) variables

Elev.1700m_top_woody_env  <- Elev.1700m_top_NWP_species2[,1:2]

Elev.1700m_top_NWP_species2$mean <- rowSums(Elev.1700m_top_NWP_species2[,2:37])/ncol(Elev.1700m_top_NWP_species2[,2:37])
Elev.1700m_top_NWP_species2

#top 5 species
Elev.1700m_top_NWP_species2[,-c(2:37)] %>%
  slice_max(mean, n=5)

##############################################################################################################

#NWP species biomass at 1700m
Elev.1700m_NWP_species <-  combine_elev.biomass_data %>%
  filter(Elev %in% "1700m",Plants %in% "non_woody") %>%
  group_by(Blocks, Treatments, Plant_sp) %>%
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
Elev.1700m_NWP_species$species_code <- sapply(Elev.1700m_NWP_species$Plant_sp, extract_species_code)
Elev.1700m_NWP_species

Elev.1700m_NWP_species <- transform(Elev.1700m_NWP_species, species_code = toupper(species_code))
Elev.1700m_NWP_species

Elev.1700m_NWP_species2 <- Elev.1700m_NWP_species %>% 
  reshape2::dcast(Blocks + Treatments ~ species_code, value.var = "Biomass",
                  fun.aggregate = sum)

Elev.1700m_NWP_species2[is.na(Elev.1700m_NWP_species2)] <- 0 #removing NAs
Elev.1700m_NWP_species2

Elev.1700m_NWP_env  <- Elev.1700m_NWP_species2[,1:2]

Elev.1700m_NWP_spp  <- Elev.1700m_NWP_species2[,-c(1:2)]


#STATISTICS
species.matrix4 <- disttransform(Elev.1700m_NWP_spp, method = 'log')
Ordination.model4  <- rda(species.matrix4 ~ Treatments + Condition(Blocks), data= Elev.1700m_NWP_env, scaling = 2)

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

#pairwise comparison (multiconstrained function in BiodiversityR package)
multiconstrained(method = "rda", 
                 formula = Elev.1700m_NWP_spp ~ Treatments + Condition(Blocks), 
                 data = Elev.1700m_NWP_env, 
                 distance = "bray", scaling=2,
                 add = TRUE)

#RDA PLOTTING for Yawan Woody Biomass
#step 1
species.matrix4 <- disttransform(Elev.1700m_NWP_spp, method = 'log')
Ordination.model4  <- rda(species.matrix4 ~ Treatments + Condition(Blocks), data= Elev.1700m_NWP_env, scaling = 2)

summary(Ordination.model4)

plot4 <- ordiplot(Ordination.model4, choices=c(1,2))

#step 2
sites.long4 <- sites.long(plot4, env.data = Elev.1700m_NWP_env)
head(sites.long4)

species.long4 <- species.long(plot4)
species.long4

axis.long4 <- axis.long(Ordination.model4, choices=c(1, 2))
axis.long4

#step 3
spec.envfit4 <- envfit(plot4, env=species.matrix4)
spec.data.envfit4 <- data.frame(r=spec.envfit4$vectors$r, p=spec.envfit4$vectors$pvals)
species.long4 <- species.long(plot4, spec.data = spec.data.envfit4)
species.long4

species.long4_filter <- species.long4 %>%
  filter(labels %in% c("SPAL","DINT","PCON","MVIM","BPIL")) %>% as.data.frame()


#plotting
#Adding centroids
centroid4 <- sites.long4 %>%
  group_by(Treatments) %>%
  summarize(axis1=mean(axis1), axis2=mean(axis2))

sites.long4_axes <- sites.long4[,1:4]


#Plots with top 10 species
Elev1700m_NWP_RDA_plot <- ggplot(sites.long4_axes, aes(x=axis1, y=axis2, color=Treatments)) + 
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
  theme_classic() +  
  theme(legend.position = c(0.15,0.15),
        legend.direction = "vertical") +
  annotate(geom="text", x=c(-0.5,-0.7,0.43,0.5), 
                       y= c(1.5,-1.2,-0.6,0.75), 
           label=c("C","I","W","WI"), size=7,
           color=c("red", "black","darkorange","blue")) +
  scale_color_manual(labels = c("C", "I","W","WI"), values = c("red", "black","darkorange","blue")) +
  theme(plot.title=element_text(hjust=0.95, vjust = -12, size =27, face = "bold")) + 
  theme(legend.title = element_text(size=19),legend.text=element_text(size=17)) +
  theme(axis.text=element_text(size=17,face = "bold"),
        axis.title=element_text(size=20,face = "bold")) +
  theme(axis.title.x = element_text(hjust=0.5, face = "bold"),
        axis.title.y = element_text(hjust=0.5,face = "bold")) +
  coord_cartesian(xlim = c(-2.5, 0.8), ylim = c(-3, 3)); Elev1700m_NWP_RDA_plot


#####---------------------------------------------------------
#combine ordination plots
#####---------------------------------------------------------
#combine ordination plots

library(grid)
library(cowplot)


RDA.combine.spp.plot <-  cowplot::plot_grid(Elev700m_WP_RDA_plot,  
                                            Elev700m_WP_RDA_plot,
                                            Elev1700m_NWP_RDA_plot,
                                            Elev1700m_WP_RDA_plot, 
                                            ncol = 2, byrow = TRUE,labels = c('A', 'B', 'C', 'D'),
                                            label_size = 22,align="hv"); RDA.combine.spp.plot


#Saving ordination plot in tiff format (dpi = 600)
ggsave("RDA.combine.spp.plot.tiff", width = 40, height = 37, units = "cm", dpi = 600)



