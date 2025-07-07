Metadata for the MS Analyses

This is the dataset and R codes for the MS titled: "Insects and non-woody plants slow down tropical forest succession: a community-wide experiment in Papua New Guinea".
Kari Sogera Iamba, Piotr Szefer, Kenneth Molem, Austin Sau, Gibson Maiah, Vojtech Novotny

(A) Datasets

Two datasets are presented in two Excel files, used for all the analyses:

[1] Plant data from the experimental plots in Combine_Sites_Biomass_2023.xlsx used for all analyses in the main MS and also for the Fig. S2 in the Supporting Information. Each row in the table represents a record of a particular plant species in a particular experimental plot, with information on plant species traits and the amount of plant biomass. Definition of columns: 

(A) Elev: elevation of the study site, 700 or 1700 m asl. 
(B) Blocks: nine blocks of four experimental plots B1 – B9, each of them in a 0.2 ha forest clearing 
(C) Treatments:  experimental treatment (C: control, I: insecticide, W: weeding, WI: weeding + insecticide). There was one plot per block for each treatment, so that the combination of columns A, B, and C defines each of the 72 plots.

(D) Plant_sp:  the name of the species or, for unidentified species, a species code 
(E) Family: plant family 
(F) Plants: classification of each species as woody or non-woody
(G) Growth_Form: classification of each species into fern, herb, grass, sedge, shrub, liana , palm or tree growth form 
(H) Status: classification of each species as native to New Guinea or alien 
(I) Biomass_kg: fresh plant biomass at the end of the experiment for each species

[2] Arthropod data from the experimental plots in CombineSite_Insects_Orders_data.xlsx used for analyses in the Supporting Information. Each row in the table represents a record of the number of individuals from a particular arthropod order found at the end of the experiment in each plot. Definition of columns: 

(A), (B), (C): see the previous data table.

(D) Order: defines the arhtropod order
(E) Guilds: classified arthropods into herbivores and other guilds
(F) Abundance: the number of individuals found in each plot at the end of the experiment

(B) R code files
Use the read_excel function from library(readxl) to load the data. The data are located in the folder called "datasets". For example: read_excel("datasets/Combine_Sites_Biomass_2023.xlsx", sheet="combine.site.biomass") [Combine_Sites_Biomass_2023 is a excel file located in datasets folder with sheet name "combine.site.biomass"]

The repository consists of seven script files used for the analyses. Five scripts (#1-5) were used for the main manuscript and two (#6-7) for the supporting information:
(1) Biomass_NWP_WP_combine.R: scripts for analyzing biomass of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI].
(2) Diversity_NWP_WP_combine.R: scripts for analyzing divesity of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI]. 
(3) LRR_WP_biomass.R: contains scripts for calculation and analyses of log response ratios (LRR) of isolated effects of insects, non-woody plants and their combination based on woody plant biomass corresponding to each treatment [C, I, W, WI].
(4) NWP+WP_RDA.R: scripts for analyzing diversity of non-woody plants (NWP) and woody plants (WP) species composition based on biomasses corresponding to each treatment [C, I, W, WI]. 
(5) alien_native_plants_WP.R: scripts for analyzing biomass of alien and native plants based on woody plant biomass corresponding to each treatment [C, I, W, WI].
(6) Insect_abundance_in_plots_supplementary.R: scripts for all insect abundance and herbivore abundance corresponding to each treatment [C, I, W, WI].
(7) alien_native_plants_biomass_C-plots_supplementary.R: contains scripts for analyzing non-woody and woody alien and native plants based on biomass corresponding to each treatment [C, I, W, WI].

(C) Information on statistical tables (in Supporting Information section)
(1) Statistical tables: TABLE S1, TABLE S2 and TABLE S3 (biomass) are extracted from Explicit contrast of the estimated marginal means (emmean) of linear mixed effects models (LMM) using the function contrast(biomass_emmean, method = contrast_list)
(2) Statistical tables: TABLE S4, TABLE S5 and TABLE S6 (diversity) are extracted from Explicit contrast of the estimated marginal means (emmean) of linear mixed effects models (LMM) using the function contrast(diversity_emmean, method = contrast_list)
(3) Statistical table: TABLE S7 is extracted by taking the summary of the LRR LMM model using the function summary function(model). Zero was included in the LRR model to set the reference to zero.
(4) Statistical table: TABLE S8 (RDA ordination) is extracted by the multiconstrained function in BiodiversityR package: _multiconstrained(method = "rda", formula = species.matrix ~ Treatments + Condition(Blocks), data = data,  scaling=2, distance = "bray"_).
(5) Statistical table: TABLE S9 (alien woody biomass and richness proportion) is extracted from Explicit contrast of the estimated marginal means (emmean) of linear mixed effects models (LMM) using the function contrast(alien_WP_emmean, method = contrast_list)









