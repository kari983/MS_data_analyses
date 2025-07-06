Metadata for the MS Analyses

This is the dataset and R codes for the MS titled: "Insects and non-woody plants slow down tropical forest succession: a community-wide experiment in Papua New Guinea".
Kari Sogera Iamba, Piotr Szefer, Kenneth Molem, Austin Sau, Gibson Maiah, Vojtech Novotny

(A). Datasets

Two datasets are used for the analyses. 
- Dataset "Combine_Sites_Biomass_2023" is used solely for the main MS results except for supporting information on alien and native plants within control plots (FIGURE S2). 
- Dataset "CombineSite_Insects_Orders_data" is used for supporting information. This dataset contains two sheets: sheet 1 (combine_insect_abundance) contains abundance for all insects and sheet 2 (herbivores) contains abundance for herbivores only.

**Below is the explaination of the data that are presented in each columns and rows of "Combine_Sites_Biomass_2023" dataset.**  
(i). Columns
(1). Column 1 ("Elev") is the abbreviation for Elevation and contains the two elevations; 700m and 1700m in which the experiments were situated. 
(2). Column 2 ("Blocks") are the actual gaps in the forest (0.2 ha) in which the treatment plots were established. 
(3). Column 3 ("Treatments") contains the treatment plots: Control (C), Insecticide treatment (I), Weeding (W), and Weeding+insecticide treatment (WI). 
(4). Column 4 ("Plant_sp") contains the plant species. 
(5). Column 5 ("Family") contains the plant species families.
(6). Column 6 ("Plants") contains non-woody and woody group.
(7). Column 7 ("Growth_Form") contains the growth forms of the plant species.
(8). Column 8 ("Status") categorises the plants to either alien or native.
(9). Column 9 ("Biomass_kg") contains the fresh weights of individal plants in kg.

(ii). Rows
(1). Rows: Each row contains a plant species with its biomass corresponding to a treatment plot at a specific elevation. Other information such as Family, Plants, Growth_Form and Status further describes the plant species. 

**Below is the explaination of the data that are presented in each columns and rows of "CombineSite_Insects_Orders_data" dataset.** 
(i). Columns
(1). Column 1 ("Elev") is the abbreviation for Elevation and contains the two elevations; 700m and 1700m in which the experiments were situated. 
(2). Column 2 ("Blocks") are the actual gaps in the forest (0.2 ha) in which the treatment plots were established. 
(3). Column 3 ("Treatments") contains the treatment plots: Control (C), Insecticide treatment (I), Weeding (W), and Weeding+insecticide treatment (WI). 
(4). Column 4 ("Order") contains the Orders of insects/herbivores. 
(5). Column 5 ("Guild") contains the feeding guilds of insects/herbivores. 
(6). Column 6 ("Abundance") contains the abundance of the insects/herbivores Orders.

(ii). Rows
(1). Rows: Each row contains an insect Order with its abundance corresponding to a treatment plot at a specific elevation. Other information such as Guild further describes the feeding characteristics of the insect order. 

(B). R codes

This repository consists of 7 script files used for the analyses. Five scripts for the main manuscript (MS) section and two for supporting information. They are located within the working directory. These scripts include:
(i). Main MS section R script files
(1). Biomass_NWP_WP_combine.R: contains scripts for analyzing biomass of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI]. 
(2). Diversity_NWP_WP_combine.R: contains scripts for analyzing divesity of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI].  
(3). LRR_WP_biomass.R: contains scripts for calculation and analyses of log response ratios (LRR) of isolated effects of insects, non-woody plants and their combination based on woody plant biomass corresponding to each treatment [C, I, W, WI]. 
(4). NWP+WP_RDA.R: contains scripts for analyzing divesity of non-woody plants (NWP) and woody plants (WP) species composition based on biomasses corresponding to each treatment [C, I, W, WI].  
(5). alien_native_plants_WP.R: contains scripts for analyzing biomass of alien and native plants based on woody plant biomass corresponding to each treatment [C, I, W, WI]. 

(ii). Supporting information R script files in MS
(1). Insect_abundance_in_plots_supplementary.R: contains scripts for all insect abundance and herbivore abundance corresponding to each treatment [C, I, W, WI]. 
(1). alien_native_plants_biomass_C-plots_supplementary.R: contains scripts for analyzing non-woody and woody alien and native plants based on biomass corresponding to each treatment [C, I, W, WI]. 

(C). Information on statistical tables (in supporting information section of MS) 
(1). Statistical tables: TABLE S1, TABLE S2 and TABLE S3 (biomass) are extracted from Explicit contrast of the estimated marginal means (emmean) of linear mixed effects models (LMM) using the function _contrast(biomass_emmean, method = contrast_list)_
(2). Statistical tables: TABLE S4, TABLE S5 and TABLE S6 (diversity) are extracted from Explicit contrast of the estimated marginal means (emmean) of linear mixed effects models (LMM) using the function _contrast(diversity_emmean, method = contrast_list)_
(3). Statistical table: TABLE S7 is extracted by taking the summary of the LRR LMM model using the function _summary function(model)_. Zero was included in the LRR model to set the reference to zero. 
(4). Statistical table: TABLE S8 (RDA ordination) is extracted by this function _adonis2(species.matrix ~ Treatments,data = predictor.matrix,method = "euclidean",permutations = 999,by = "onedf")_ and _pairwise.adonis(species.matrix, Treatments)_ from adonis2 and pairwiseAdonis R packages. 
(5). Statistical table: TABLE S9 (alien woody biomass and richness proportion) is extracted from Explicit contrast of the estimated marginal means (emmean) of linear mixed effects models (LMM) using the function _contrast(alien_WP_emmean, method = contrast_list)_

# summary
summary(mod_Elev.700m_WP_LRR)

Explicit contrast of the estimated marginal means (emmean) of the linear mixed effects models (LMM) using the function _contrast(emmean, method = contrast_list)_







