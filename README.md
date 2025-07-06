Metadata for the MS Analyses

This is the dataset and R codes for the MS titled: "Insects and non-woody plants slow down tropical forest succession: a community-wide experiment in Papua New Guinea".
Kari Sogera Iamba, Piotr Szefer, Kenneth Molem, Austin Sau, Gibson Maiah, Vojtech Novotny

(A). Datasets

Three datasets are used for the analyses. Dataset "Combine_Sites_Biomass_2023" is used solely for the main MS results except for supporting information of alien and native plants within control plots (Figure S2). While datasets "All_Insects_abundance" and "Herbivores_abundance" are used for supporting information. 

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

**Below is the explaination of the data that are presented in each columns and rows of "All_Insects_abundance" dataset.** 
(i). Columns
(1). Column 1 ("Elev") is the abbreviation for Elevation and contains the two elevations; 700m and 1700m in which the experiments were situated. 
(2). Column 2 ("Blocks") are the actual gaps in the forest (0.2 ha) in which the treatment plots were established. 
(3). Column 3 ("Treatments") contains the treatment plots: Control (C), Insecticide treatment (I), Weeding (W), and Weeding+insecticide treatment (WI). 
(4). Column 4 ("Order") contains the Orders of insects. 
(5). Column 5 ("Guild") contains the feeding guilds of insects. 
(6). Column 6 ("Abundance") contains the abundance of the insect Orders.

(ii). Rows
(1). Rows: Each row contains an insect Order with its abundance corresponding to a treatment plot at a specific elevation. Other information such as Guild further describes the feeding characteristics of the insect order. 

**Below is the explaination of the data that are presented in each columns and rows of "Herbivores_abundance" dataset.** 
(i). Columns
(1). Column 1 ("Elev") is the abbreviation for Elevation and contains the two elevations; 700m and 1700m in which the experiments were situated. 
(2). Column 2 ("Blocks") are the actual gaps in the forest (0.2 ha) in which the treatment plots were established. 
(3). Column 3 ("Treatments") contains the treatment plots: Control (C), Insecticide treatment (I), Weeding (W), and Weeding+insecticide treatment (WI). 
(4). Column 4 ("Order") contains the Orders of herbivores. 
(5). Column 5 ("Guild") contains the feeding guilds of herbivores. 
(6). Column 6 ("Abundance") contains the abundance of the herbivore Orders.

(ii). Rows
(1). Rows: Each row contains an herbivore Order with its abundance corresponding to a treatment plot at a specific elevation. Other information such as Guild further describes the feeding characteristics of the herbivore order. 

(B). R codes

This repository consists of 7 script files used for the analyses. Five scripts for the main MS section and two for supporting information. They are located within the working directory. These scripts include:
(i). Main MS section
(1). Biomass_NWP_WP_combine.R: contains scripts for analyzing biomass of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI]. 
(2). Diversity_NWP_WP_combine.R: contains scripts for analyzing divesity of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI].  
(3). LRR_WP_biomass.R: contains scripts for calculation and analyses of log response ratios (LRR) of isolated effects of insects, non-woody plants and their combination based on woody plant biomass corresponding to each treatment [C, I, W, WI]. 
(4). NWP+WP_RDA.R: contains scripts for analyzing divesity of non-woody plants (NWP) and woody plants (WP) species composition based on biomasses corresponding to each treatment [C, I, W, WI].  
(5). alien_native_plants_WP.R: contains scripts for analyzing biomass of alien and native plants based on woody plant biomass corresponding to each treatment [C, I, W, WI]. 

(ii). Supporting information
(1). Insect_abundance_in_plots_supplementary.R: contains scripts for analyzing biomass of non-woody plants (NWP), woody plants (WP) and their combined biomasses corresponding to each treatment [C, I, W, WI]. 




